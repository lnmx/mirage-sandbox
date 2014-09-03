open Lwt
open Printf
open V1_LWT

module Main (C:CONSOLE) (S:Cohttp_lwt.Server) = struct

  let start c http =

    let handler request body =
      S.respond_string ~status:`OK ~body:"hello world!\n" ()
    in

    let log request =
        let meth = Cohttp.Code.string_of_method (S.Request.meth request) in
        let path = Uri.path (S.Request.uri request) in
        let vers = Cohttp.Code.string_of_version (S.Request.version request) in
        C.log c (Printf.sprintf "http: %s %s %s" meth path vers);

        let headers = Cohttp.Header.to_list (S.Request.headers request) in
        List.iter (fun (k,v) -> C.log c (Printf.sprintf "    %s: %s" k v) ) headers
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
