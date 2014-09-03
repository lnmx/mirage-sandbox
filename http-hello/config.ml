open Mirage

let net =
  match get_mode() with
  | `Xen  -> `Direct
  | `Unix -> `Socket

let net_addr = [Ipaddr.V4.any]

let net_port = 
  match get_mode() with
  | `Xen  -> 80
  | `Unix -> 8080

let stack console =
  match net with
  | `Direct -> direct_stackv4_with_dhcp console tap0
  | `Socket -> socket_stackv4 console net_addr

let server =
  http_server net_port (stack default_console)

let main =
  foreign "Unikernel.Main" (console @-> http @-> job)

let () =
  register "http" [
    main $ default_console $ server
  ]
