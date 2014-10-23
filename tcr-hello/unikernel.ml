open Lwt
open Printf
open V1_LWT

module Main (C:CONSOLE) (CLK: V1.CLOCK) (S:Cohttp_lwt.Server) = struct

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

    let buf = 
      Tcr_buf.create
    in

    let handler request body =
      S.respond_string ~status:`OK ~body:"hello world!\n" ()
    in

    let trace event =
      C.log c (Tcr_event.to_json event) 
      (*Tcr_buf.push buf event*)
    in

    let trace_gc () =
        let time = ts () in
        let stat = Gc.stat () in
        trace (Tcr_event.counter ~cat:(Some "memory") ~pid:(Some 1) ~tid:(Some 1) time "gc" [ 
            ("minor_collections", (float_of_int stat.Gc.minor_collections) ) ; 
            ("major_collection", (float_of_int stat.Gc.major_collections) )  ;
            ("compactions", (float_of_int stat.Gc.compactions) ) 
        ]);
        trace (Tcr_event.counter ~cat:(Some "memory") ~pid:(Some 1) ~tid:(Some 1) time "memory" [ 
            ("free_words", (float_of_int stat.Gc.free_words) ) ;
            ("live_words", (float_of_int stat.Gc.live_words) ) 
        ])
    in

    let dump () =
        Tcr_buf.dump buf (fun ln -> C.log c ln)
    in

    let trace_handler request body =
      trace (Tcr_event.dur_begin ~pid:(Some 1) ~tid:(Some 1) (ts ()) "request");
      lwt out = handler request body in
      trace (Tcr_event.dur_end ~pid:(Some 1) ~tid:(Some 1) (ts ()) "request");
      trace_gc ();
      return out
    in

    (*
    let dump_writeln push ln =
      C.log c (Printf.sprintf "writeln %s" ln);
      push (Some (ln ^ "\n"))
    in

    let dump_writer push =
      C.log c "dump writer";
      Tcr_buf.dump buf (fun ln -> dump_writeln push ln);
      push None;
      return ()
    in

    let start_dump_writer push =
      Lwt.async (fun () -> dump_writer push)
    in
    *)

    let dump_handler request body =
      trace_gc();
      dump();
      S.respond_string ~status:`OK ~body:"dumped!\n" ()

      (*
      C.log c "send trace";
      let (stream, push) = Lwt_stream.create () in
      let body = Cohttp_lwt_body.of_stream stream in
      let resp = Cohttp.Response.make ~status:`OK ~flush:true () in
      lwt out = dump_writer push in
      (*let _ = start_dump_writer push in*)
      return (resp, body)
      *)
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

    let start =
      trace_gc();
      trace (Tcr_event.process_name 1 "mir-http");
      trace (Tcr_event.thread_name 1 1 "main");
      trace (Tcr_event.dur_complete ~pid:(Some 1) ~tid:(Some 1) (ts ()) "start" 100000);
    in

    http { S.callback; conn_closed }

end
