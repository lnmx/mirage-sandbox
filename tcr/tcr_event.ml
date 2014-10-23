
open Printf

type argument =
    [ `NullArg
    | `BoolArg of bool
    | `FloatArg of float
    | `IntArg of int
    | `StringArg of string
    | `ArrayArg of argument list
    | `ObjectArg of (string * argument) list ]

type event = {
    (* display name *)
    name : string;

    (* categories, comma-delimited *)
    cat : string option;

    (* phase (event type) *)
    ph : string;

    (* timestamp in microseconds *)
    ts : int;

    (* process id *)
    pid : int option;

    (* thread id *)
    tid : int option;
   
    (* scope, for phase=Instant values are g=global, p=process, t=thread *)
    s : string option;

    (* duration in microseconds, for phase=Complete *)
    dur : int option;

    (* async/flow/object id *)
    id : string option;

    (* arguments *)
    args : argument;
}

let default =
    { name = ""; cat = None; ph = ""; ts = 0; pid = None; tid = None; s = None; dur = None; id = None; args = `NullArg }

let dur_begin ?(cat=None) ?(pid=None) ?(tid=None) ts name =
    { default with name; cat; ph = "B"; ts; pid; tid }

let dur_end ?(cat=None) ?(pid=None) ?(tid=None) ts name =
    { default with name; cat; ph = "E"; ts; pid; tid }

let dur_complete ?(cat=None) ?(pid=None) ?(tid=None) ts name dur =
    { default with name; cat; ph = "X"; ts; pid; tid; dur = (Some dur) }

let async_begin ?(cat=None) ?(pid=None) ?(tid=None) ts name id =
    { default with name; cat; ph = "S"; ts; pid; tid; id = (Some id) }

let async_end ?(cat=None) ?(pid=None) ?(tid=None) ts name id =
    { default with name; cat; ph = "F"; ts; pid; tid; id = (Some id) }

let async_instant ?(cat=None) ?(pid=None) ?(tid=None) ts name id step =
    { default with name; cat; ph = "T"; ts; pid; tid; id = (Some id); args = `ObjectArg [ ("step", `StringArg step) ] }

let flow_begin ?(cat=None) ?(pid=None) ?(tid=None) ts name id =
    { default with name; cat; ph = "s"; ts; pid; tid; id = (Some id) }

let flow_end ?(cat=None) ?(pid=None) ?(tid=None) ts name id =
    { default with name; cat; ph = "f"; ts; pid; tid; id = (Some id) }

let flow_instant ?(cat=None) ?(pid=None) ?(tid=None) ts name id =
    { default with name; cat; ph = "t"; ts; pid; tid; id = (Some id) }

type metric =
    (string * float)

type metrics =
    metric list

let metrics_to_args m =
    `ObjectArg (List.map (fun (k,v) -> (k, `FloatArg v)) m) 

let counter ?(cat=None) ?(pid=None) ?(tid=None) ts name (m:metrics) =
    let args = metrics_to_args m in
    { default with name; cat; ph = "C"; ts; pid; tid; args }

let process_name pid name =
    { default with name = "process_name"; ph = "M"; pid = (Some pid); args = `ObjectArg [ ("name", `StringArg name) ] }

let process_sort pid sort =
    { default with name = "process_sort_index"; ph = "M"; pid = (Some pid); args = `ObjectArg [ ("sort_index", `FloatArg (float_of_int sort)) ] }

let process_labels pid labels =
    { default with name = "process_labels"; ph = "M"; pid = (Some pid); args = `ObjectArg [ ("labels", `StringArg labels) ] }

let thread_name pid tid name =
    { default with name = "thread_name"; ph = "M"; pid = (Some pid); tid = (Some tid); args = `ObjectArg [ ("name", `StringArg name) ] }

let thread_sort pid tid sort =
    { default with name = "thread_sort_index"; ph = "M"; pid = (Some pid); tid = (Some tid); args = `ObjectArg [ ("sort_index", `FloatArg (float_of_int sort)) ] }

let instant ?(cat=None) ?(pid=None) ?(tid=None) ts name scope =
    { default with name; cat; ph = "I"; ts; pid; tid; s = (Some scope) }

let sample ?(cat=None) ?(pid=None) ?(tid=None) ts name =
    { default with name; cat; ph = "P"; ts; pid; tid; }

let object_created ?(cat=None) ?(pid=None) ?(tid=None) ts name id =
    { default with name; cat; ph = "N"; ts; pid; tid; id = (Some id) }

let object_destroyed ?(cat=None) ?(pid=None) ?(tid=None) ts name id =
    { default with name; cat; ph = "D"; ts; pid; tid; id = (Some id) }

let object_snapshot ?(cat=None) ?(pid=None) ?(tid=None) ts name id snapshot =
    { default with name; cat; ph = "O"; ts; pid; tid; id = (Some id); args = `ObjectArg [ ("snapshot", snapshot) ] }


let merge_args a _ = a
(*
    match (a, b) with
    | `ObjectArg a, `ObjectArg b -> `ObjectArg a
    | _ -> failwith "must be ObjectArgs"
*)

let object_ref key id =
    `ObjectArg [ (key, `ObjectArg [ ("id_ref", `StringArg id) ] ) ]

let with_object_ref (key:string) (id:string) event =
    let oref = object_ref key id in
    let oarg = merge_args oref event.args in
    { event with args = oarg }

let to_string e =
    sprintf "% 6d %s: %s" e.ts e.ph e.name

let to_json_optional name value = 
    let open Ezjsonm in
    match value with
    | (Some v) -> [ (name, `String v) ]
    | None -> []

let to_json_optional_int name value = 
    let open Ezjsonm in
    match value with
    | (Some v) -> [ (name, `Float (float_of_int v)) ]
    | None -> []

let rec to_json_arg a = 
    let open Ezjsonm in
    let enc_kv (key,value) = (key, (to_json_arg value)) in
    match a with
    | `NullArg -> `Null
    | `BoolArg v -> `Bool v
    | `FloatArg v -> `Float v
    | `IntArg v -> `Float (float_of_int v)
    | `StringArg v -> `String v
    | `ArrayArg v -> `A (List.map to_json_arg v)
    | `ObjectArg v -> `O (List.map enc_kv v)

let to_json_args name a =
    let open Ezjsonm in
    match a with
    | `NullArg -> []
    | _ -> [ (name, (to_json_arg a) )]

let to_json_data e =
    let open Ezjsonm in
    let attrs = [] in
    let name = [ ( "name", `String e.name ) ] in
    let cat = to_json_optional "cat" e.cat in 
    let ph = [ ( "ph", `String e.ph ) ] in
    let ts = [ ( "ts", `Float (float_of_int e.ts) ) ] in
    let pid = to_json_optional_int "pid" e.pid in
    let tid = to_json_optional_int "tid" e.tid in
    let s = to_json_optional "s" e.s in
    let dur = to_json_optional_int "dur" e.dur in
    let id = to_json_optional "id" e.id in
    let args = to_json_args "args" e.args in
    let attrs = List.concat [ attrs ; name ; ph ; ts ; pid ; tid ; cat ; s ; dur ; id ; args ] in
    `O attrs

let to_json e =
    let open Ezjsonm in
    Ezjsonm.to_string ~minify:true (to_json_data e) ^ ","

let dump ?(printer=(fun ln -> printf "%s" ln)) events =
    printer "[\n";
    List.iter (fun e -> printer (to_json e)) events
