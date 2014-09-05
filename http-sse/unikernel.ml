open Lwt
open Printf
open V1_LWT

module Main (C:CONSOLE) (S:Cohttp_lwt.Server) = struct

  let start c http =

    let serve_index request body =
      S.respond_string ~status:`OK ~body:"hello, world!\r\n" ()
    in

    let generate_stream push =
        for_lwt i = 0 to 10 do
            OS.Time.sleep 1.0 >>
            return ( push (Some (Printf.sprintf "data: hello\r\ndata: %d\r\n\r\n" i) ) )
        done
    in

    let close_stream push =
        return ( push None )
    in

    let serve_stream request body =
      let (stream, push) = Lwt_stream.create () in
      let headers = Cohttp.Header.init_with "Content-Type" "text/event-stream" in
      let body = Cohttp_lwt_body.of_stream stream in
      let resp = Cohttp.Response.make ~status:`OK ~headers () in
      let foo = Lwt.async (fun () -> generate_stream push >> close_stream push) in
      return (resp, body)
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

    let conn_closed conn_id () =
      let cid = Cohttp.Connection.to_string conn_id in
      C.log c (Printf.sprintf "conn %s closed" cid)
    in

    http { S.callback; conn_closed }

end
