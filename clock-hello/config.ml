open Mirage

let main = foreign "Unikernel.Main" (console @-> clock @-> job)

let () =
  register "clock" [
    main $ default_console $ default_clock
  ]
