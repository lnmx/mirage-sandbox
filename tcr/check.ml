
open Core.Std
open Tcr_event

let example =
    [
        Tcr_event.process_name 1 "my-process" ;
        Tcr_event.process_labels 1 "my process label" ;
        Tcr_event.process_sort 1 1 ;

        Tcr_event.thread_name 1 1 "duration / stacks" ;
        Tcr_event.thread_sort 1 1 1 ;
        Tcr_event.dur_complete ~pid:(Some 1) ~tid:(Some 1) 25000 "start" 75000 ;
        Tcr_event.dur_begin ~pid:(Some 1) ~tid:(Some 1) 150000 "setup" ;
        Tcr_event.dur_begin ~pid:(Some 1) ~tid:(Some 1) 170000 "alloc" ;
        Tcr_event.dur_begin ~pid:(Some 1) ~tid:(Some 1) 190000 "brk" ;
        Tcr_event.dur_end ~pid:(Some 1) ~tid:(Some 1) 240000 "brk" ;
        Tcr_event.dur_begin ~pid:(Some 1) ~tid:(Some 1) 242000 "zero" ;
        Tcr_event.dur_end ~pid:(Some 1) ~tid:(Some 1) 249000 "zero" ;
        Tcr_event.dur_end ~pid:(Some 1) ~tid:(Some 1) 290000 "alloc" ;
        Tcr_event.dur_end ~pid:(Some 1) ~tid:(Some 1) 450000 "setup" ;

        Tcr_event.thread_name 1 2 "gc" ;
        Tcr_event.thread_sort 1 2 2 ;
        Tcr_event.dur_complete ~pid:(Some 1) ~tid:(Some 2) 480000 "gc" 20000 ;

        Tcr_event.counter ~pid:(Some 1) ~cat:(Some "gc") 0 "memory" [ ("allocated", 0.); ("reserved", 2048. ) ] ;
        Tcr_event.counter ~pid:(Some 1) ~cat:(Some "gc") 100000 "memory" [ ("allocated", 1000.); ("reserved", 2048. ) ] ;
        Tcr_event.counter ~pid:(Some 1) ~cat:(Some "gc") 200000 "memory" [ ("allocated", 2000.); ("reserved", 2048. ) ] ;
        Tcr_event.counter ~pid:(Some 1) ~cat:(Some "gc") 300000 "memory" [ ("allocated", 3000.); ("reserved", 4096. ) ] ;
        Tcr_event.counter ~pid:(Some 1) ~cat:(Some "gc") 400000 "memory" [ ("allocated", 4000.); ("reserved", 4096. ) ] ;
        Tcr_event.instant ~pid:(Some 1) ~cat:(Some "gc") 480000 "gc" "p" ;
        Tcr_event.counter ~pid:(Some 1) ~cat:(Some "gc") 500000 "memory" [ ("allocated", 1100.); ("reserved", 4096. ) ] ;

        Tcr_event.instant ~pid:(Some 1) 400000 "process-instant" "p" ;
        Tcr_event.instant ~pid:(Some 1) ~tid:(Some 1) 410000 "thread-instant" "t" ;
        Tcr_event.instant 420000 "global-instant" "g" ;

        Tcr_event.thread_name 1 3 "flow 1" ;
        Tcr_event.thread_name 1 4 "flow 2" ;
        Tcr_event.flow_begin ~pid:(Some 1) ~tid:(Some 3) 125000 "ex-flow" "f-100" ;
        Tcr_event.flow_end ~pid:(Some 1) ~tid:(Some 4) 195000 "ex-flow" "f-100" ;

        Tcr_event.flow_begin ~pid:(Some 1) ~tid:(Some 3) 225000 "ex-flow" "f-100" ;
        Tcr_event.flow_instant ~pid:(Some 1) ~tid:(Some 4) 285000 "ex-flow" "f-100" ;
        Tcr_event.flow_end ~pid:(Some 1) ~tid:(Some 3) 345000 "ex-flow" "f-100" ;

        Tcr_event.thread_name 1 5 "async simple" ;
        Tcr_event.dur_complete ~pid:(Some 1) ~tid:(Some 5) 10000 "setup" 2000 ;
        Tcr_event.async_begin ~pid:(Some 1) ~tid:(Some 5) 125000 "async-simple" "a1" ;
        Tcr_event.async_end ~pid:(Some 1) ~tid:(Some 6) 250000 "async-simple" "a1" ; 

        Tcr_event.thread_name 1 6 "async instant" ;
        Tcr_event.dur_complete ~pid:(Some 1) ~tid:(Some 6) 10000 "setup" 2000 ;
        Tcr_event.async_begin ~pid:(Some 1) ~tid:(Some 6) 125000 "async-inst" "a2" ;
        Tcr_event.async_instant ~pid:(Some 1) ~tid:(Some 6) 200000 "async-inst" "a2" "setup" ;
        Tcr_event.async_instant ~pid:(Some 1) ~tid:(Some 6) 300000 "async-inst" "a2" "exec" ;
        Tcr_event.async_end ~pid:(Some 1) ~tid:(Some 6) 390000 "async-inst" "a2" ; 

        Tcr_event.thread_name 1 7 "async nest" ;
        Tcr_event.dur_complete ~pid:(Some 1) ~tid:(Some 7) 10000 "setup" 2000 ;
        Tcr_event.async_begin ~pid:(Some 1) ~tid:(Some 7) 125000 "async-nest" "a3" ;
        Tcr_event.async_begin ~pid:(Some 1) ~tid:(Some 7) 195000 "async-nest-1" "a3" ;
        Tcr_event.async_end ~pid:(Some 1) ~tid:(Some 7) 245000 "async-nest-1" "a3" ;
        Tcr_event.async_begin ~pid:(Some 1) ~tid:(Some 7) 275000 "async-nest-2" "a3" ;
        Tcr_event.async_end ~pid:(Some 1) ~tid:(Some 7) 405000 "async-nest-2" "a3" ;
        Tcr_event.async_end ~pid:(Some 1) ~tid:(Some 7) 525000 "async-nest" "a3" ;

        Tcr_event.thread_name 1 8 "sample" ;
        Tcr_event.dur_complete ~pid:(Some 1) ~tid:(Some 8) 10000 "setup" 2000 ;
        Tcr_event.sample ~pid:(Some 1) ~tid:(Some 8) 110000 "foo" ;
        Tcr_event.sample ~pid:(Some 1) ~tid:(Some 8) 210000 "bar" ;
        Tcr_event.sample ~pid:(Some 1) ~tid:(Some 8) 310000 "foo" ;
        Tcr_event.sample ~pid:(Some 1) ~tid:(Some 8) 410000 "bar" ;

        Tcr_event.thread_name 1 9 "object" ;
        Tcr_event.dur_complete ~pid:(Some 1) ~tid:(Some 9) 10000 "setup" 2000 ;
        Tcr_event.object_created ~pid:(Some 1) ~tid:(Some 9) 70000 "MyObject" "o1" ;
        Tcr_event.object_snapshot ~pid:(Some 1) ~tid:(Some 9) 80000 "MyObject" "o1" (`ObjectArg [ ("stuff", `StringArg "junk" ) ] ) ;
        Tcr_event.dur_complete ~pid:(Some 1) ~tid:(Some 9) 85000 "frob" 35000 |> Tcr_event.with_object_ref "MyObject" "o1" ;
        Tcr_event.object_snapshot ~pid:(Some 1) ~tid:(Some 9) 230000 "MyObject" "o1" (`ObjectArg [ ("stuff", `StringArg "blah" ) ] ) ;
        Tcr_event.dur_complete ~pid:(Some 1) ~tid:(Some 9) 260000 "frob" 35000 |> Tcr_event.with_object_ref "MyObject" "o1" ;
        Tcr_event.object_destroyed ~pid:(Some 1) ~tid:(Some 9) 270000 "MyObject" "o1" ;
        Tcr_event.object_created ~pid:(Some 1) ~tid:(Some 9) 290000 "MyStruct" "o1" ;
        Tcr_event.object_snapshot ~pid:(Some 1) ~tid:(Some 9) 290000 "MyStruct" "o1" (`ObjectArg [ ("ref", `StringArg "foo" ) ] ) ;
        Tcr_event.object_snapshot ~pid:(Some 1) ~tid:(Some 9) 320000 "MyStruct" "o1" (`ObjectArg [ ("ref", `StringArg "bar" ) ] ) ;
        Tcr_event.object_destroyed ~pid:(Some 1) ~tid:(Some 9) 350000 "MyStruct" "o1" ;
    ]


let () =
    let buf = Tcr_buf.create in
    List.iter example ~f:(fun e -> Tcr_buf.push buf e);
    Tcr_buf.dump buf (fun ln -> printf "%s\n" ln);
