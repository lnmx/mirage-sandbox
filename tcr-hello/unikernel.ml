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

    let pr line =
      C.log c line
    in

    let buf = 
      Tcr_buf.create
    in

    let dump () =
      Tcr_buf.dump buf pr
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

    let dump_handler request body =
      dump ();
      S.respond_string ~status:`OK ~body:"dumped!\n" ()
    in

    (* HTTP callback *)
    let callback conn_id request body =
      let path = Uri.path (S.Request.uri request) in
      match path with
      | "/dump" -> dump_handler request body
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
