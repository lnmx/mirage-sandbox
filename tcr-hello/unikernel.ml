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
      lwt () = OS.Time.sleep 0.2 in
      S.respond_string ~status:`OK ~body:"hello world!\n" ()
    in

    let trace event =
      Tcr_buf.push buf event
    in

    let trace_handler request body =
      trace (Tcr_event.dur_begin ~pid:(Some 1) ~tid:(Some 1) (ts ()) "request");
      lwt out = handler request body in
      trace (Tcr_event.dur_end ~pid:(Some 1) ~tid:(Some 1) (ts ()) "request");
      return out
    in

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

    let dump_handler request body =
      C.log c "send trace";
      let (stream, push) = Lwt_stream.create () in
      let body = Cohttp_lwt_body.of_stream stream in
      let resp = Cohttp.Response.make ~status:`OK () in
      lwt out = dump_writer push in
      (*let _ = start_dump_writer push in*)
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

    let start =
      trace (Tcr_event.dur_complete ~pid:(Some 1) ~tid:(Some 1) (ts ()) "start" 100000);
    in

    http { S.callback; conn_closed }

end
