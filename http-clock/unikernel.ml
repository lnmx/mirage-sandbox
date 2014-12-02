open Lwt
open Printf
open V1_LWT

module Main (C:CONSOLE) (CLK: V1.CLOCK) (S:Cohttp_lwt.Server) = struct

  let start c clock http =

    let handler request body =
      S.respond_string ~status:`OK ~body:(Printf.sprintf "time is %f\n" (CLK.time ())) ()
    in

    (* HTTP callback *)
    let callback conn_id request body =
      handler request body
    in

    let conn_closed (_, conn_id) () =
      let cid = Cohttp.Connection.to_string conn_id in
      C.log c (Printf.sprintf "conn %s closed" cid)
    in

    http { S.callback; conn_closed }

end
