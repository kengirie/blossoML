(** blob 配信の停止監視

    背景: クライアントが応答の途中で切断する・受信を止めると、Piaf 0.2.0 /
    httpun 0.2.0 では本文送信側が書き込み完了を永久に待ち続け、接続
    （ソケット）と配信中のファイルが解放されない。HTTP/2 でもストリームの
    リセットや接続断で同じことが起きる。

    送信側は書き込み完了を待ってから次のチャンクを取りに来るので、
    「応答が終わっていないのに一定時間取りに来ない」配信を停止とみなす。

    - 停止した配信はファイルを閉じる（[on_stall]）
    - HTTP/1.x: 1接続に配信は1つなので、停止したら接続を切る
    - HTTP/2: 1接続を複数のストリームが共有し、フロー制御による一時停止も
      正当な動作なので、配信中のストリームが「すべて」停止しているか、
      停止したストリームが [max_stalled_streams] 本以上たまった場合に接続を切る

    接続を切るときは接続ごとのスイッチを失敗させる。止まったファイバーごと
    接続が片付けられ、Piaf はその失敗をログに記録して他の接続の処理を続ける。

    監視ファイバーは接続ごとに1本で、配信中の応答がなくなると終了する。 *)

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

type mono_clock = Eio.Time.Mono.ty Eio.Std.r

type entry = {
  label : string;
  mutable last_take : Mtime.t;
  mutable finished : bool;
  mutable stalled : bool;
  on_stall : unit -> unit;
}

type conn = {
  sw : Eio.Switch.t;
  clock : mono_clock;
  multiplexed : bool;  (** HTTP/2 *)
  mutable entries : entry list;
  mutable watching : bool;
}

let idle_seconds clock (e : entry) =
  Mtime.span e.last_take (Eio.Time.Mono.now clock) |> Mtime.Span.to_float_ns
  |> fun ns -> ns /. 1e9

let fail conn exn =
  Eio.Switch.fail conn.sw exn

(** 1回分の判定。接続を切った・監視対象がなくなった場合は [`Stop] *)
let check conn =
  conn.entries <- List.filter (fun e -> not e.finished) conn.entries;
  if conn.entries = [] then `Stop
  else begin
    List.iter (fun e ->
      if not e.stalled && idle_seconds conn.clock e >= stall_timeout then begin
        e.stalled <- true;
        Eio.traceln "Client stalled for %.0fs while streaming %s" stall_timeout e.label;
        e.on_stall ()
      end
    ) conn.entries;
    let total = List.length conn.entries in
    let stalled = List.length (List.filter (fun e -> e.stalled) conn.entries) in
    let should_close =
      stalled > 0
      && ((not conn.multiplexed) || stalled = total || stalled >= max_stalled_streams)
    in
    if should_close then begin
      Eio.traceln "Closing stalled connection (%d of %d responses stalled)" stalled total;
      fail conn (Client_stalled (List.hd conn.entries).label);
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

(** 接続ごとの監視状態。Piaf は接続に任意の状態を持たせる手段を提供しないため、
    接続中は一意なクライアントアドレスをキーに保持し、接続終了時に取り除く *)
let registry : (Eio.Net.Sockaddr.stream, conn) Hashtbl.t = Hashtbl.create 64

let for_connection ~sw ~clock ~client_address ~multiplexed =
  match Hashtbl.find_opt registry client_address with
  | Some conn when conn.sw == sw -> conn
  | _ ->
      let conn = { sw; clock; multiplexed; entries = []; watching = false } in
      Hashtbl.replace registry client_address conn;
      Eio.Switch.on_release sw (fun () ->
        match Hashtbl.find_opt registry client_address with
        | Some c when c == conn -> Hashtbl.remove registry client_address
        | _ -> ());
      conn

(** 配信1件を監視対象に登録する [Storage_intf.watch] を作る *)
let watch_response conn ~label : Storage_intf.watch = fun ~on_stall ->
  let e = {
    label;
    last_take = Eio.Time.Mono.now conn.clock;
    finished = false;
    stalled = false;
    on_stall;
  } in
  conn.entries <- e :: conn.entries;
  ensure_watching conn;
  {
    Storage_intf.taken = (fun () -> e.last_take <- Eio.Time.Mono.now conn.clock);
    finished = (fun () -> e.finished <- true);
    truncated = (fun () ->
      e.finished <- true;
      (* HTTP/1.x で Content-Length より短く終わると、同じ接続の次の応答が
         本文の続きとして読まれてしまうため、接続を切る *)
      if not conn.multiplexed then begin
        Eio.traceln "Response for %s ended early, closing connection" label;
        fail conn (Response_truncated label)
      end);
  }
