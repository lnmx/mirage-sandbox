
open Printf

let create =
    ref [ Tcr_event.process_name 1 "foo" ]

let count buf = 
    List.length !buf

let dump buf printer =
    printer "[";
    List.iter (fun e -> printer (Tcr_event.to_json e)) !buf

let push buf event =
    buf := List.concat [ !buf ; [ event ] ];
