open Lwt

let start streamer request body =
  let (stream, push) = Lwt_stream.create () in
  let headers = Cohttp.Header.init_with "Content-Type" "text/event-stream" in
  let body = Cohttp_lwt_body.of_stream stream in
  let resp = Cohttp.Response.make ~status:`OK ~headers () in
  let _ = Lwt.async (fun () -> streamer push) in
  return (resp, body)

let send client data =
  return ( client (Some data) )

let close client =
  return ( client None )

