open Lwt
open Printf
open V1_LWT
open Sse

module Main (C:CONSOLE) (S:Cohttp_lwt.Server) = struct

  let start c http =

    let serve_index request body =
      S.respond_string ~status:`OK ~body:"hello, world!\r\n" ()
    in

    let streamer client =
      OS.Time.sleep 1.0 >>
      Sse.send client "hello!\r\n\r\n" >>
      OS.Time.sleep 3.0 >>
      Sse.send client "goodbye!\r\n\r\n" >>
      Sse.close client
    in

    let serve_stream request body =
      Sse.start streamer request body
    in

    let handler request body =
      let verb = S.Request.meth request in
      let path = Uri.path (S.Request.uri request) in
      match verb, path with
      | `GET, "/"       -> serve_index request body
      | `GET, "/stream" -> serve_stream request body
      | _,    _         -> S.respond_not_found ()
    in

    let callback conn_id request body =
      handler request body
    in

    let conn_closed (_, conn_id) () =
      let cid = Cohttp.Connection.to_string conn_id in
      C.log c (Printf.sprintf "conn %s closed" cid)
    in

    http { S.callback; conn_closed }

end
