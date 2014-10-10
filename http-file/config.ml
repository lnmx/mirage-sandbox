open Mirage

let fs_mode = 
  try match String.lowercase (Unix.getenv "FS") with
    | "fat" -> `Fat
    | _     -> `Crunch
  with Not_found -> `Crunch

let fat_ro dir =
  kv_ro_of_fs (fat_of_files ~dir ())

let fs = match fs_mode with
  | `Fat    -> fat_ro "./htdocs"
  | `Crunch -> crunch "./htdocs"

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
  foreign "Unikernel.Main" (console @-> kv_ro @-> http @-> job)

let () =
  register "http" [
    main $ default_console $ fs $ http_server
  ]
