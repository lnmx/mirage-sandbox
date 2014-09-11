open Lwt
open Printf
open V1_LWT

module Main (C:CONSOLE) (FS:KV_RO) (S:Cohttp_lwt.Server) = struct

  let start c fs http =

    let read_file path =
      FS.size fs path
      >>= function
      | `Error (FS.Unknown_key _) -> fail (Failure ("read " ^ path))
      | `Ok size ->
        FS.read fs path 0 (Int64.to_int size)
        >>= function
        | `Error (FS.Unknown_key _) -> fail (Failure ("read " ^ path))
        | `Ok buffer -> return (Cstruct.copyv buffer)
    in

    let serve_file path =
      try_lwt
        read_file path
        >>= fun body ->
        S.respond_string ~status:`OK ~body ()
      with exn ->
        S.respond_not_found ()
    in

    let handler request body =
      let path = Uri.path (S.Request.uri request) in
      serve_file path
    in

    let log request =
        let meth = Cohttp.Code.string_of_method (S.Request.meth request) in
        let path = Uri.path (S.Request.uri request) in
        C.log c (Printf.sprintf "http: %s %s" meth path)
    in

    (* HTTP callback *)
    let callback conn_id request body =
      log request;
      handler request body
    in

    let conn_closed conn_id () =
      let cid = Cohttp.Connection.to_string conn_id in
      C.log c (Printf.sprintf "conn %s closed" cid)
    in

    http { S.callback; conn_closed }

end
