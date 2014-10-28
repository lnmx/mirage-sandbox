
(* modeled after github.com/talex5/mirage-profile: Copyright (C) 2014, Thomas Leonard *)

module Event = struct 

    type time = float

    type argument =
        [ `NullArg
        | `BoolArg of bool
        | `FloatArg of float
        | `IntArg of int
        | `StringArg of string
        | `ArrayArg of argument list
        | `ObjectArg of (string * argument) list ]

    type t = {
        name : string;
        cat : string option;
        ph : string;
        ts : time;
        pid : int option;
        tid : int option;
        s : string option;
        dur : time option;
        id : string option;
        args : argument;
    }

    let default = {
        name = "";
        cat = None;
        ph = "";
        ts = 0.;
        pid = None;
        tid = None;
        s = None;
        dur = None;
        id = None;
        args = `NullArg
    }

    let to_json event =
        let open Ezjsonm in

        let js_string name value =
            [ ( name, `String value ) ]
        in
        let js_string_opt name value =
            match value with
            | (Some value) -> js_string name value
            | None -> []
        in
        let js_int name value =
            [ ( name, `Float (float_of_int value) ) ]
        in
        let js_int_opt name value =
            match value with
            | (Some value) -> js_int name value
            | None -> []
        in
        let js_float name value =
            [ ( name, `Float value ) ]
        in
        let js_float_opt name value = 
            match value with
            | (Some value) -> js_float name value
            | None -> []
        in
        let rec js_argument_value value =
            let enc_kv (key,value) = (key, (js_argument_value value)) in
            match value with
            | `NullArg -> `Null
            | `BoolArg v -> `Bool v
            | `FloatArg v -> `Float v
            | `IntArg v -> `Float (float_of_int v)
            | `StringArg v -> `String v
            | `ArrayArg v -> `A (List.map js_argument_value v)
            | `ObjectArg v -> `O (List.map enc_kv v)
        in
        let js_argument name value =
            match value with
            | `NullArg -> []
            | `ObjectArg _ -> [ ( name, (js_argument_value value) ) ]
            | _ -> failwith "args must be NullArg or ObjectArg"
        in
        let attrs = List.concat [ 
            js_string     "name" event.name ;
            js_string_opt "cat"  event.cat ;
            js_string     "ph"   event.ph ;
            js_float      "ts"   event.ts ;
            js_int_opt    "pid"  event.pid ;
            js_int_opt    "tid"  event.tid ;
            js_string_opt "s"    event.s ;
            js_float_opt  "dur"  event.dur ;
            js_string_opt "id"   event.id ;
            js_argument   "args" event.args ;
        ] in
        let event_obj = `O attrs in
        Ezjsonm.to_string ~minify:true event_obj

end

module Log = struct
    open Event

    external timestamp : unit -> float = "unix_gettimeofday"

    type t = {
        t0 : float;
        log : Event.t array;
        mutable head : int;
        mutable full : bool;
        mutable enabled : bool;
    }

    let create ~size =
        let log = {
            t0 = timestamp ();
            log = Array.make size default;
            head = 0;
            full = false;
            enabled = true;
        } in
        log

    let event_log = ref (create ~size:1)

    let start ~size =
        event_log := create ~size

    let pause () = 
        !event_log.enabled <- false

    let resume () =
        !event_log.enabled <- true

    let step log =
        let next = log.head + 1 in
        let next =
            if next >= Array.length log.log then (
                log.full <- true;
                0
            ) else next in
        log.head <- next

    let add event =
        let log = !event_log in
        if log.enabled then (
            log.log.(log.head) <- { event with ts=( (timestamp () -. log.t0) *. 1000000.) };
        step log
        )

    let log_begin name pid tid =
        add { default with name; ph="B"; pid=(Some pid); tid=(Some tid); }

    let log_end name pid tid =
        add { default with name; ph="E"; pid=(Some pid); tid=(Some tid); }

    let log_instant_process name pid = 
        add { default with name; ph="I"; pid=(Some pid); s=(Some "p") }

    let log_instant_thread name pid tid = 
        add { default with name; ph="I"; pid=(Some pid); tid=(Some tid); s=(Some "t") }

    let log_instant_global name = 
        add { default with name; ph="I"; s=(Some "g") }

    let log_async_begin name id pid tid =
        add { default with name; ph="S"; pid=(Some pid); tid=(Some tid); id=(Some id) }

    let log_async_end name id pid tid =
        add { default with name; ph="F"; pid=(Some pid); tid=(Some tid); id=(Some id) }

    let log_async_instant name id step pid tid =
        add { default with name; ph="T"; pid=(Some pid); tid=(Some tid); id=(Some id);
            args = `ObjectArg [ ("step", `StringArg step) ] }

    let log_flow_begin name id pid tid =
        add { default with name; ph="s"; pid=(Some pid); tid=(Some tid); id=(Some id) }

    let log_flow_end name id pid tid =
        add { default with name; ph="f"; pid=(Some pid); tid=(Some tid); id=(Some id) }

    let log_flow_instant name id pid tid =
        add { default with name; ph="t"; pid=(Some pid); tid=(Some tid); id=(Some id) }

    let process_meta pid name label sort =
        add { default with name = "process_name"; ph="M"; pid=(Some pid); args=`ObjectArg [("name", `StringArg name)] };
        add { default with name = "process_labels"; ph="M"; pid=(Some pid); args=`ObjectArg [("labels", `StringArg label)] };
        add { default with name = "process_sort_index"; ph="M"; pid=(Some pid); args=`ObjectArg [("sort_index", `IntArg sort)] }

    let thread_meta pid tid name sort =
        add { default with name = "thread_name"; ph="M"; pid=(Some pid); tid=(Some tid); args=`ObjectArg [("name", `StringArg name)] };
        add { default with name = "thread_sort_index"; ph="M"; pid=(Some pid); tid=(Some tid); args=`ObjectArg [("sort_index", `IntArg sort)] }

    let log_counter name pid key value =
        let args = `ObjectArg [ (key, `FloatArg value) ] in
        add { default with name; pid=(Some pid); ph="C"; args }

    let log_counters name pid metrics =
        let args = `ObjectArg (List.map (fun (k,v) -> (k, `FloatArg v)) metrics) in
        add { default with name; pid=(Some pid); ph="C"; args }


    (* dur_complete "X" with dur=time *)
    (* sample "P" *)
    (* object_created "N" id *)
    (* object_destroyed "D" id *)
    (* object_snapshot "O" id args="snapshot": argument *)
    (* with_object_ref args=key={"id_ref": id} *)

    let output f =
        let log = !event_log in
        if log.full then (
            let n = Array.length log.log in
            for i = log.head to n - 1 do
                f (i-log.head) n log.log.(i)
            done;
            let offset = n - log.head in
            for i = 0 to log.head - 1 do
                f (i+offset) n log.log.(i)
            done;
        ) else (
            let n = log.head - 1 in
            for i = 0 to n do
                f i n log.log.(i)
            done
        )

    let output_json f =
        let f_item i n e =
            let js = to_json e in
            let line = if i < n then js ^ ",\n" else js ^ "\n" in
            (* f ( (string_of_int i) ^ " " ^ (string_of_int n) ^ " " ^ line ) *)
            f line
        in
        f "[\n";
        output f_item;
        f "]\n"
end

type event = Event.t

let start = Log.start
let pause = Log.pause
let resume = Log.resume
let log_begin = Log.log_begin 
let log_end = Log.log_end
let log_instant_global = Log.log_instant_global
let log_instant_thread = Log.log_instant_thread
let log_instant_process = Log.log_instant_process
let log_async_begin = Log.log_async_begin
let log_async_end = Log.log_async_end
let log_async_instant = Log.log_async_instant
let log_flow_begin = Log.log_flow_begin
let log_flow_end = Log.log_flow_end
let log_flow_instant = Log.log_flow_instant
let output_json = Log.output_json

let process_meta = Log.process_meta
let thread_meta = Log.thread_meta

let log_counter = Log.log_counter
let log_counters = Log.log_counters

let () =
    let open Printf in
    let open Unix in
    start ~size:100;
    process_meta 1 "unikernel" "my unikernel" 1;
    thread_meta 1 1 "gc" 2;
    for i = 0 to 3 do
        log_counter "loop" 1 "i" (float_of_int i);
        log_counters "progress" 1 [ ("done", (float_of_int i)) ; ("to go"), (float_of_int (3-i)) ];
        log_begin "stuff" 1 1;
        sleep 1;
        log_end "stuff" 1 1
    done;
    output_json (fun line -> Printf.printf "%s" line)
