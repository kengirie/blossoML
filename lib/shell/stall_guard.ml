(** blob 配信の停止監視と同時配信数の制限

    背景: クライアントが応答の途中で切断する・受信を止めると、Piaf 0.2.0 /
    httpun 0.2.0 では本文送信側が書き込み完了を永久に待ち続け、接続
    （ソケット）と配信中のファイルが解放されない。HTTP/2 でもストリームの
    リセットや接続断で同じことが起きる。

    送信側は書き込み完了を待ってから次のチャンクを取りに来るので、
    「応答が終わっていないのに一定時間取りに来ない」配信を停止とみなす。

    - 停止した配信はファイルを閉じる（[on_stall]）。再開されたら開き直す
    - HTTP/1.x: 1接続に配信は1つなので、停止したら接続を切る
    - HTTP/2: 1接続を複数のストリームが共有し、フロー制御による一時停止も
      正当な動作なので、次のいずれかの場合だけ接続を切る
      - 停止したストリームが [max_stalled_streams] 本以上たまった
      - 配信中のストリームがすべて停止し、処理中のリクエストもなく、
        接続全体が [stall_timeout] 秒何もしていない（死んだ接続）

    接続を切るときは接続ごとのスイッチを失敗させる。止まったファイバーごと
    接続が片付けられ、Piaf はその失敗をログに記録して他の接続の処理を続ける。

    さらに、ファイル記述子を使い切られないよう、未完了の配信数を
    接続ごと（[max_active_per_connection]）とサーバー全体の開いている
    配信ファイル数（[max_open_files]）で制限し、超えた要求は登録時に拒否する。

    監視ファイバーは接続ごとに1本で、配信中の応答がなくなると終了する。
    状態は単一ドメイン（Piaf の既定 domains = 1）で操作される前提で、
    排他制御はしていない。 *)

exception Client_stalled of string
exception Response_truncated of string

(** 次のチャンクが取りに来られないまま経過してよい最大秒数。
    送信バッファが大きいと、一定速度で読み続ける遅いクライアントでも
    取りに来る間隔は数十秒に開くため、死んだ接続の回収に十分な範囲で長めに取る *)
let stall_timeout = 120.0

(** 監視の間隔（秒） *)
let check_interval = 5.0

(** HTTP/2 で、他のストリームが動いていても接続を切る停止ストリーム数 *)
let max_stalled_streams = 8

(** 1接続あたりの未完了の配信数の上限 *)
let max_active_per_connection = 32

(** サーバー全体で同時に開いておく配信ファイル数の上限 *)
let max_open_files = 512

type mono_clock = Eio.Time.Mono.ty Eio.Std.r

type entry = {
  label : string;
  mutable last_take : Mtime.t;
  mutable finished : bool;
  mutable stalled : bool;
  mutable holding : bool;  (** 配信ファイルを開いている（[open_files] に計上済み） *)
  on_stall : unit -> unit;
}

type conn = {
  sw : Eio.Switch.t;
  clock : mono_clock;
  multiplexed : bool;  (** HTTP/2 *)
  mutable entries : entry list;  (** 未完了の配信 *)
  mutable watching : bool;
  mutable inflight : int;  (** 処理中のリクエスト数（ハンドラ実行中） *)
  mutable last_activity : Mtime.t;  (** リクエストの開始・終了、チャンクの取得 *)
}

(** サーバー全体で開いている配信ファイル数 *)
let open_files = ref 0

let acquire e = if not e.holding then begin e.holding <- true; incr open_files end
let release e = if e.holding then begin e.holding <- false; decr open_files end

let now conn = Eio.Time.Mono.now conn.clock

let seconds_since conn t =
  Mtime.Span.to_float_ns (Mtime.span t (now conn)) /. 1e9

let touch conn = conn.last_activity <- now conn

(** 1回分の判定。接続を切った・監視対象がなくなった場合は [`Stop] *)
let check conn =
  conn.entries <- List.filter (fun e -> not e.finished) conn.entries;
  if conn.entries = [] then `Stop
  else begin
    List.iter (fun e ->
      if not e.stalled && seconds_since conn e.last_take >= stall_timeout then begin
        e.stalled <- true;
        release e;
        Eio.traceln "Client stalled for %.0fs while streaming %s" stall_timeout e.label;
        e.on_stall ()
      end
    ) conn.entries;
    let total = List.length conn.entries in
    let stalled = List.length (List.filter (fun e -> e.stalled) conn.entries) in
    let should_close =
      stalled > 0
      && ((not conn.multiplexed)
          || stalled >= max_stalled_streams
          || (stalled = total
              && conn.inflight = 0
              && seconds_since conn conn.last_activity >= stall_timeout))
    in
    if should_close then begin
      Eio.traceln "Closing stalled connection (%d of %d responses stalled)" stalled total;
      Eio.Switch.fail conn.sw (Client_stalled (List.hd conn.entries).label);
      `Stop
    end else `Continue
  end

let rec watch conn =
  match check conn with
  | `Stop ->
      conn.watching <- false;
      `Stop_daemon
  | `Continue ->
      Eio.Time.Mono.sleep conn.clock check_interval;
      watch conn

let ensure_watching conn =
  if not conn.watching then begin
    conn.watching <- true;
    Eio.Fiber.fork_daemon ~sw:conn.sw (fun () -> watch conn)
  end

(** 接続ごとの状態。Piaf は接続に任意の状態を持たせる手段を提供しないため、
    接続中は一意なクライアントアドレスをキーに保持し、接続終了時に取り除く *)
let registry : (Eio.Net.Sockaddr.stream, conn) Hashtbl.t = Hashtbl.create 64

let for_connection ~sw ~clock ~client_address ~multiplexed =
  match Hashtbl.find_opt registry client_address with
  | Some conn when conn.sw == sw -> conn
  | _ ->
      let conn = {
        sw; clock; multiplexed;
        entries = []; watching = false;
        inflight = 0; last_activity = Eio.Time.Mono.now clock;
      } in
      Hashtbl.replace registry client_address conn;
      Eio.Switch.on_release sw (fun () ->
        (* 接続終了時にはファイルもスイッチと共に閉じられるので計上を戻す *)
        List.iter release conn.entries;
        conn.entries <- [];
        match Hashtbl.find_opt registry client_address with
        | Some c when c == conn -> Hashtbl.remove registry client_address
        | _ -> ());
      conn

(** リクエスト1件の処理中であることを記録する *)
let with_request conn f =
  conn.inflight <- conn.inflight + 1;
  touch conn;
  Fun.protect f ~finally:(fun () ->
    conn.inflight <- conn.inflight - 1;
    touch conn)

(** 配信1件を監視対象に登録する [Storage_intf.watch] を作る。
    同時配信数の上限を超える場合は登録せずに [Error] を返す *)
let watch_response conn ~label : Storage_intf.watch = fun ~on_stall ->
  if List.length (List.filter (fun e -> not e.finished) conn.entries)
     >= max_active_per_connection then
    Error "Too many concurrent downloads on this connection"
  else if !open_files >= max_open_files then
    Error "Server is busy, try again later"
  else begin
    let e = {
      label;
      last_take = now conn;
      finished = false;
      stalled = false;
      holding = false;
      on_stall;
    } in
    acquire e;
    conn.entries <- e :: conn.entries;
    ensure_watching conn;
    let finish () = e.finished <- true; release e in
    Ok {
      Storage_intf.taken = (fun () ->
        e.last_take <- now conn;
        touch conn);
      finished = finish;
      truncated = (fun () ->
        finish ();
        (* HTTP/1.x で Content-Length より短く終わると、同じ接続の次の応答が
           本文の続きとして読まれてしまうため、接続を切る *)
        if not conn.multiplexed then begin
          Eio.traceln "Response for %s ended early, closing connection" label;
          Eio.Switch.fail conn.sw (Response_truncated label)
        end);
      resume = (fun () ->
        if e.holding then true
        else if !open_files >= max_open_files then false
        else begin
          acquire e;
          e.stalled <- false;
          true
        end);
    }
  end
