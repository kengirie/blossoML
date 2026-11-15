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
  let progress = G.watch_response conn ~label ~on_stall:(fun () -> stalled := true) in
  progress, stalled

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
  let flag = ref true in
  let r = with_conn ~multiplexed:false (fun ~clock conn ->
    let p, stalled = register conn "a" in
    keep_taking clock p ~every:(G.stall_timeout /. 2.) ~total:(G.stall_timeout *. 4.);
    p.finished ();
    flag := !stalled) in
  check outcome "connection kept" (`Ok ()) r;
  check bool "not stalled" false !flag

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

let tests = [
  test_case "HTTP/1: stalled response closes connection" `Quick test_http1_stalled_closes;
  test_case "HTTP/1: steadily progressing response kept" `Quick test_http1_active_kept;
  test_case "idle keep-alive after finished response kept" `Quick test_idle_after_finish_kept;
  test_case "watcher stops when no active responses" `Quick test_watcher_stops_when_idle;
  test_case "HTTP/2: one paused stream, others active -> kept" `Quick test_h2_one_paused_others_active_kept;
  test_case "HTTP/2: all streams stalled -> closed" `Quick test_h2_all_stalled_closes;
  test_case "HTTP/2: too many stalled streams -> closed" `Quick test_h2_many_stalled_closes;
  test_case "HTTP/1: truncated response closes connection" `Quick test_truncated_http1_closes;
  test_case "HTTP/2: truncated response keeps connection" `Quick test_truncated_h2_kept;
]
