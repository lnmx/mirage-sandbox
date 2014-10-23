open Mirage

let net =
  match get_mode() with
  | `Xen  -> `Direct
  | `Unix -> `Socket

let net_addr = [Ipaddr.V4.any]

let http_port = 
  match get_mode() with
  | `Xen  -> 80
  | `Unix -> 8080

let stack console =
  match net with
  | `Direct -> direct_stackv4_with_dhcp console tap0
  | `Socket -> socket_stackv4 console net_addr

let server =
  conduit_direct (stack default_console)

let http_server =
    let mode = `TCP (`Port http_port) in
    http_server mode server

let main =
  foreign "Unikernel.Main" (console @-> clock @-> http @-> job)

let () =
  add_to_ocamlfind_libraries ["ezjsonm"];
  add_to_opam_packages ["ezjsonm"];

  register "http" [
    main $ default_console $ default_clock $ http_server
  ]
