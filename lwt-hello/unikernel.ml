open Lwt

module Main (C: V1_LWT.CONSOLE) = struct

  let key = Lwt.new_key ()

  let get_key () =
    match Lwt.get key with
    | Some s -> s
    | None -> "<none>"

  let print_key obs c =
    let value = get_key () in
    C.log c (Printf.sprintf "%s has %s" obs value)

  let start c =
    C.log c "hello, world";
    print_key "start" c;
    Lwt.with_value key (Some "foo") (fun () -> print_key "thread" c);
    Lwt.return_unit

end
