open Mirage

let main = foreign "Unikernel.Main" (console @-> job)

let () =
  register "hello-lwt" [
    main $ default_console
  ]
