open Lwt
open Printf
open V1_LWT

module Main (C:CONSOLE) (CLK: V1.CLOCK) (S:Cohttp_lwt.Server) = struct

  let () = 
      Tcr.start ~size:10000

  let gc_pid = 1
  let my_pid = 2

  let log_gc_data () =
    let stat = Gc.stat () in
      Tcr.log_counters "gc memory" gc_pid [
        ("free_words", (float_of_int stat.Gc.free_words) ) ;
        ("live_words", (float_of_int stat.Gc.live_words) ) 
      ];
      Tcr.log_counters "gc actions" gc_pid [
        ("minor_collections", (float_of_int stat.Gc.minor_collections) ) ; 
        ("major_collection", (float_of_int stat.Gc.major_collections) ) ;
        ("compactions", (float_of_int stat.Gc.compactions) ) 
      ]

  let log_gc_event () =
    Tcr.log_instant_global "gc collection";
    log_gc_data ()

  let rec gc_logger () =
    lwt _ = OS.Time.sleep 1.0 in
    log_gc_data ();
    gc_logger ()

  let log_setup () =
    Tcr.reset();
    Tcr.process_meta gc_pid "gc" "" 1;
    Tcr.thread_meta gc_pid gc_pid "gc" 1;
    Tcr.process_meta my_pid "unikernel" "" 1;
    Tcr.thread_meta my_pid my_pid "main" 1;
    log_gc_data ()

  let start c clock http =

    let t0 =
      CLK.time ()
    in

    let ts () =
      let t1 = CLK.time () in
      let dt = t1 -. t0 in
      let us = int_of_float (dt *. 1000000.) in
      us
    in

    let handler request body =
      S.respond_string ~status:`OK ~body:"hello world!\n" ()
    in

    let trace_handler request body =
      Tcr.log_begin "request" my_pid 1;
      lwt out = handler request body in
      Tcr.log_end "request" my_pid 1;
      return out
    in

    let dump_writer push =
      Tcr.pause ();
      Tcr.output_json (fun ln -> push (Some ln));
      push None;
      log_setup ();
      return ()
    in

    let start_dump_writer push =
      Lwt.async (fun () -> dump_writer push)
    in

    let dump_handler request body =
      let (stream, push) = Lwt_stream.create () in
      let body = Cohttp_lwt_body.of_stream stream in
      let resp = Cohttp.Response.make ~status:`OK ~flush:true () in
      let _ = start_dump_writer push in
      return (resp, body)
    in

    (* HTTP callback *)
    let callback conn_id request body =
      let path = Uri.path (S.Request.uri request) in
      match path with
      | "/test.trace" -> dump_handler request body
      | _ -> trace_handler request body
    in

    let conn_closed (_, conn_id) () =
      let cid = Cohttp.Connection.to_string conn_id in
      C.log c (Printf.sprintf "conn %s closed" cid)
    in

    let () =
        log_setup ();
        ignore (Gc.create_alarm log_gc_event);
        Lwt.async gc_logger;
        ()
    in

    http { S.callback; conn_closed }

end
