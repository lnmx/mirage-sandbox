open Lwt
open Printf
open V1_LWT

module Main (C:CONSOLE) (FS:KV_RO) (S:Cohttp_lwt.Server) = struct

  let start c fs http =

    (* Buffer file *)

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

    let serve_file_buffered path =
      C.log c (Printf.sprintf "serve_file buffered %s" path);
      read_file path
      >>= fun body ->
      S.respond_string ~status:`OK ~body ()
    in

    (* Stream file, push *)

    let read_file_chunk path offset length =
      FS.read fs path offset length
      >>= function
      | `Error (FS.Unknown_key _) -> fail (Failure ("read " ^ path))
      | `Ok buffer -> return (Cstruct.copyv buffer)
    in

    let rec push_reader path offset step push =
        read_file_chunk path offset step 
        >>= function
        | "" -> return (push None)
        | buf -> (push (Some buf)); push_reader path (offset+step) step push
    in

    let start_push_reader path step push =
        Lwt.async (fun () -> push_reader path 0 step push);
        1
    in

    let serve_file_stream_push path =
      C.log c (Printf.sprintf "serve_file stream_push %s" path);
      FS.size fs path
      >>= function
      | `Error (FS.Unknown_key _) -> fail (Failure ("read " ^ path))
      | `Ok size ->
        let (stream, push) = Lwt_stream.create () in
        let body = Cohttp_lwt_body.of_stream stream in
        let resp = Cohttp.Response.make ~status:`OK () in
        let _ = start_push_reader path 4096 push in
        return (resp, body)
    in

    (* Stream file, pull *)

    let pull_reader path step =
      let offset = ref 0 in
      fun () ->
        read_file_chunk path !offset step
        >>= function
        | "" -> return None
        | buf -> offset := !offset + step; return (Some buf)
    in

    let serve_file_stream_pull path =
      C.log c (Printf.sprintf "serve_file stream_pull %s" path);
      let step = 4096 in
      let stream = Lwt_stream.from (pull_reader path step) in
      let body = Cohttp_lwt_body.of_stream stream in
      let resp = Cohttp.Response.make ~status:`OK () in
      return (resp, body)
    in

    let serve_file mode path =
      match mode with
        | Some "push" -> serve_file_stream_push path
        | Some "pull" -> serve_file_stream_pull path
        | Some "buff" -> serve_file_buffered path
        | _ -> serve_file_buffered path
    in

    let handler request body =
      try_lwt
        let path = Uri.path (S.Request.uri request) in
        let mode = Uri.get_query_param (S.Request.uri request) "mode" in
        serve_file mode path
      with exn ->
        S.respond_not_found ()
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

    let conn_closed (_, conn_id) () =
      let cid = Cohttp.Connection.to_string conn_id in
      C.log c (Printf.sprintf "conn %s closed" cid)
    in

    http { S.callback; conn_closed }

end
