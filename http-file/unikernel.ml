open Lwt
open Printf
open V1_LWT

module Main (C:CONSOLE) (FS:KV_RO) (S:Cohttp_lwt.Server) = struct

  let step = 8192

  let read fs path offset length =
      FS.read fs path offset length
      >>= function
      | `Error _ -> fail (Failure ("read " ^ path))
      | `Ok buffer -> return (Cstruct.copyv buffer)


  (* Read the entire file and return it as a response *)
  module Fserv_full = struct

    let serve c fs path size =
        C.log c (Printf.sprintf "fserv_full %s" path);
        read fs path 0 size
        >>= fun body ->
        let resp = Cohttp.Response.make ~status:`OK () in
        return (resp, `String body)
  end

  (* Cohttp pulls buffers as needed through an Lwt stream *)
  module Fserv_stream_pull = struct

    let pull fs path size step =
        let offset = ref 0 in
        fun () ->
          read fs path !offset step
          >>= function
          | "" -> return None
          | buf -> offset := !offset + step; return (Some buf)

    let serve c fs path size =
        C.log c (Printf.sprintf "fserv_stream_pull %s" path);
        let stream = Lwt_stream.from (pull fs path size step) in
        let body = Cohttp_lwt_body.of_stream stream in
        let resp = Cohttp.Response.make ~status:`OK () in
        return (resp, body)
  end

  (* Push buffers into an Lwt stream for Cohttp to consume *)
  module Fserv_stream_push = struct

    let rec copy fs path size offset step send =
        read fs path offset step
        >>= function
        | "" -> return (send None)
        | buf -> (send (Some buf)); copy fs path size (offset+step) step send

    let start fs path size send =
        Lwt.async (fun () -> copy fs path size 0 step send)

    let serve c fs path size =
        C.log c (Printf.sprintf "fserv_stream_push %s" path);
        let (stream, send) = Lwt_stream.create () in
        let body = Cohttp_lwt_body.of_stream stream in
        let resp = Cohttp.Response.make ~status:`OK () in
        let _ = start fs path size send in
        return (resp, body)
  end


  let start c fs http =

    let check_file path =
      FS.size fs path
      >>= function
      | `Error _ -> fail (Failure ("read " ^ path))
      | `Ok size -> return (Int64.to_int size)
    in

    let serve_file mode path =
      lwt size = check_file path in
      match mode with
        | Some "push"     -> Fserv_stream_push.serve c fs path size
        | Some "pull"     -> Fserv_stream_pull.serve c fs path size
        | Some "full" | _ -> Fserv_full.serve c fs path size
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
