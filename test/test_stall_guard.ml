open Alcotest
module G = Blossom_shell.Stall_guard

(* Eio_mock.Backend.run_full の単調時計は、全ファイバーが待機中になると
   自動で次のタイマーまで進むため、長い待ち時間も即座にテストできる *)
let with_conn ~multiplexed f =
  Eio_mock.Backend.run_full (fun env ->
    let clock = (Eio.Stdenv.mono_clock env :> G.mono_clock) in
    match
      Eio.Switch.run (fun sw ->
        let client_address = `Tcp (Eio.Net.Ipaddr.V4.loopback, 40000) in
        let conn = G.for_connection ~sw ~clock ~client_address ~multiplexed in
        f ~clock conn)
    with
    | v -> `Ok v
    | exception G.Client_stalled _ -> `Closed_stalled
    | exception G.Response_truncated _ -> `Closed_truncated)

let register conn label =
  let stalled = ref false in
  match G.watch_response conn ~label ~on_idle:(fun () -> stalled := true) with
  | Ok progress -> progress, stalled
  | Error e -> failf "registration rejected: %s" e

let sleep clock s = Eio.Time.Mono.sleep clock s

(* 一定間隔で次のチャンクを取りに来る（動いている配信） *)
let keep_taking clock (p : Blossom_shell.Storage_intf.progress) ~every ~total =
  let t = ref 0.0 in
  while !t < total do
    sleep clock every; p.taken (); t := !t +. every
  done

let outcome = testable (fun fmt -> function
  | `Ok () -> Format.pp_print_string fmt "Ok"
  | `Closed_stalled -> Format.pp_print_string fmt "Closed_stalled"
  | `Closed_truncated -> Format.pp_print_string fmt "Closed_truncated") ( = )

let test_http1_stalled_closes () =
  let flag = ref false in
  let r = with_conn ~multiplexed:false (fun ~clock conn ->
    let _p, stalled = register conn "a" in
    Fun.protect ~finally:(fun () -> flag := !stalled) (fun () ->
      sleep clock (G.stall_timeout +. 10.))) in
  check outcome "connection closed" `Closed_stalled r;
  check bool "file released" true !flag

let test_http1_active_kept () =
  (* 取りに来る間隔が [stall_timeout] 未満なら、開いて閉じてを繰り返しても接続は切らない *)
  let r = with_conn ~multiplexed:false (fun ~clock conn ->
    let p, _ = register conn "a" in
    keep_taking clock p ~every:(G.stall_timeout /. 2.) ~total:(G.stall_timeout *. 4.);
    p.finished ()) in
  check outcome "connection kept" (`Ok ()) r

let test_frequent_takes_keep_file () =
  let flag = ref true in
  let r = with_conn ~multiplexed:false (fun ~clock conn ->
    let p, idle_closed = register conn "a" in
    keep_taking clock p ~every:(G.file_idle_timeout /. 2.) ~total:(G.stall_timeout *. 2.);
    flag := !idle_closed;
    p.finished ()) in
  check outcome "connection kept" (`Ok ()) r;
  check bool "file never closed" false !flag

let test_idle_after_finish_kept () =
  let r = with_conn ~multiplexed:false (fun ~clock conn ->
    let p, _ = register conn "a" in
    p.taken (); p.finished ();
    sleep clock (G.stall_timeout *. 3.)) in
  check outcome "idle keep-alive kept" (`Ok ()) r

let test_watcher_stops_when_idle () =
  let r = with_conn ~multiplexed:false (fun ~clock conn ->
    let p, _ = register conn "a" in
    check bool "watching while active" true conn.G.watching;
    p.finished ();
    sleep clock (G.check_interval *. 2.);
    check bool "watcher stopped" false conn.G.watching) in
  check outcome "ok" (`Ok ()) r

let test_h2_one_paused_others_active_kept () =
  let paused_released = ref false in
  let r = with_conn ~multiplexed:true (fun ~clock conn ->
    let pa, sa = register conn "paused" in
    let pb, sb = register conn "active" in
    keep_taking clock pb ~every:10. ~total:(G.stall_timeout *. 2.);
    paused_released := !sa;
    check bool "active not stalled" false !sb;
    pa.finished (); pb.finished ()) in
  check outcome "connection kept" (`Ok ()) r;
  check bool "paused stream's file released" true !paused_released

let test_h2_all_stalled_closes () =
  let r = with_conn ~multiplexed:true (fun ~clock conn ->
    let _ = register conn "a" in
    let _ = register conn "b" in
    sleep clock (G.stall_timeout +. 10.)) in
  check outcome "connection closed" `Closed_stalled r

let test_h2_many_stalled_closes () =
  let r = with_conn ~multiplexed:true (fun ~clock conn ->
    for i = 1 to G.max_stalled_streams do ignore (register conn (string_of_int i)) done;
    let p, _ = register conn "active" in
    keep_taking clock p ~every:10. ~total:(G.stall_timeout *. 3.)) in
  check outcome "connection closed" `Closed_stalled r

let test_truncated_http1_closes () =
  let r = with_conn ~multiplexed:false (fun ~clock conn ->
    let p, _ = register conn "a" in
    p.truncated ();
    sleep clock 1.) in
  check outcome "connection closed" `Closed_truncated r

let test_truncated_h2_kept () =
  let r = with_conn ~multiplexed:true (fun ~clock conn ->
    let p, _ = register conn "a" in
    p.truncated ();
    sleep clock 1.) in
  check outcome "connection kept" (`Ok ()) r

let test_h2_paused_with_other_requests_kept () =
  let released = ref false in
  let r = with_conn ~multiplexed:true (fun ~clock conn ->
    let p, sa = register conn "paused" in
    (* 同じ接続で短いリクエストが2秒ごとに来る（配信の監視には現れない） *)
    let t = ref 0.0 in
    while !t < G.stall_timeout *. 2.5 do
      G.with_request conn (fun () -> ());
      sleep clock 2.; t := !t +. 2.
    done;
    released := !sa;
    p.finished ()) in
  check outcome "connection kept" (`Ok ()) r;
  check bool "paused stream's file released" true !released

let test_h2_paused_with_long_request_kept () =
  let r = with_conn ~multiplexed:true (fun ~clock conn ->
    let p, _ = register conn "paused" in
    (* 長いアップロードなど、処理中のリクエストがある *)
    G.with_request conn (fun () -> sleep clock (G.stall_timeout *. 2.5));
    p.finished ()) in
  check outcome "connection kept" (`Ok ()) r

let test_per_connection_limit () =
  let r = with_conn ~multiplexed:true (fun ~clock:_ conn ->
    let ps = List.init G.max_active_per_connection (fun i -> fst (register conn (string_of_int i))) in
    (match G.watch_response conn ~label:"over" ~on_idle:ignore with
     | Ok _ -> fail "expected rejection over the per-connection limit"
     | Error _ -> ());
    (* 1件終われば受け付ける *)
    (List.hd ps).finished ();
    (match G.watch_response conn ~label:"again" ~on_idle:ignore with
     | Ok p -> p.finished ()
     | Error e -> failf "expected acceptance after a slot freed: %s" e);
    List.iter (fun (p : Blossom_shell.Storage_intf.progress) -> p.finished ()) ps) in
  check outcome "ok" (`Ok ()) r;
  check int "open file count restored" 0 !G.open_files

let test_global_open_files_limit () =
  let r = Eio_mock.Backend.run_full (fun env ->
    let clock = (Eio.Stdenv.mono_clock env :> G.mono_clock) in
    Eio.Switch.run (fun sw ->
      let conn i =
        G.for_connection ~sw ~clock ~multiplexed:true
          ~client_address:(`Tcp (Eio.Net.Ipaddr.V4.loopback, 41000 + i)) in
      let n_conns = G.max_open_files / G.max_active_per_connection in
      for i = 0 to n_conns - 1 do
        let c = conn i in
        for j = 1 to G.max_active_per_connection do ignore (register c (string_of_int j)) done
      done;
      check int "open files at the limit" G.max_open_files !G.open_files;
      match G.watch_response (conn n_conns) ~label:"over" ~on_idle:ignore with
      | Ok _ -> `Accepted
      | Error _ -> `Rejected)) in
  check bool "rejected at the global limit" true (r = `Rejected);
  check int "open file count restored after connections end" 0 !G.open_files

let test_resume_after_stall () =
  let r = with_conn ~multiplexed:true (fun ~clock conn ->
    let pa, sa = register conn "paused" in
    let pb, _ = register conn "active" in
    keep_taking clock pb ~every:10. ~total:(G.stall_timeout +. 20.);
    check bool "paused stalled and released" true !sa;
    check int "only the active stream holds a file" 1 !G.open_files;
    check bool "resume allowed" true (pa.resume ());
    check int "resumed stream holds a file again" 2 !G.open_files;
    pa.taken ();
    keep_taking clock pb ~every:10. ~total:(G.stall_timeout /. 2.);
    pa.finished (); pb.finished ()) in
  check outcome "connection kept" (`Ok ()) r;
  check int "open file count restored" 0 !G.open_files

let test_idle_file_released_connection_kept () =
  let r = with_conn ~multiplexed:false (fun ~clock conn ->
    let p, released = register conn "a" in
    check int "holds a file" 1 !G.open_files;
    (* 短い中断: ファイルは閉じるが、接続は切らない *)
    sleep clock (G.file_idle_timeout +. G.check_interval +. 1.);
    check bool "file closed after idle period" true !released;
    check int "slot returned" 0 !G.open_files;
    (* 再開すると開き直す *)
    check bool "resume allowed" true (p.resume ());
    p.taken ();
    check int "slot reacquired" 1 !G.open_files;
    keep_taking clock p ~every:5. ~total:30.;
    p.finished ()) in
  check outcome "connection kept" (`Ok ()) r;
  check int "open file count restored" 0 !G.open_files

let tests = [
  test_case "HTTP/1: stalled response closes connection" `Quick test_http1_stalled_closes;
  test_case "HTTP/1: steadily progressing response kept" `Quick test_http1_active_kept;
  test_case "frequent takes keep the file open" `Quick test_frequent_takes_keep_file;
  test_case "idle keep-alive after finished response kept" `Quick test_idle_after_finish_kept;
  test_case "watcher stops when no active responses" `Quick test_watcher_stops_when_idle;
  test_case "HTTP/2: one paused stream, others active -> kept" `Quick test_h2_one_paused_others_active_kept;
  test_case "HTTP/2: all streams stalled -> closed" `Quick test_h2_all_stalled_closes;
  test_case "HTTP/2: too many stalled streams -> closed" `Quick test_h2_many_stalled_closes;
  test_case "HTTP/1: truncated response closes connection" `Quick test_truncated_http1_closes;
  test_case "HTTP/2: truncated response keeps connection" `Quick test_truncated_h2_kept;
  test_case "HTTP/2: paused stream + other short requests -> kept" `Quick test_h2_paused_with_other_requests_kept;
  test_case "HTTP/2: paused stream + long request in progress -> kept" `Quick test_h2_paused_with_long_request_kept;
  test_case "per-connection download limit" `Quick test_per_connection_limit;
  test_case "global open file limit" `Quick test_global_open_files_limit;
  test_case "resume after stall reacquires the file" `Quick test_resume_after_stall;
  test_case "idle file released without closing connection" `Quick test_idle_file_released_connection_kept;
]
