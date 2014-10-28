
(* modeled after github.com/talex5/mirage-profile: Copyright (C) 2014, Thomas Leonard *)

val start : size:int -> unit
val pause : unit -> unit
val resume : unit -> unit
val reset : unit -> unit

val log_begin : string -> int -> int -> unit
val log_end : string -> int -> int -> unit

val log_instant_thread : string -> int -> int -> unit
val log_instant_process : string -> int -> unit
val log_instant_global : string -> unit

val log_async_begin : string -> string -> int -> int -> unit
val log_async_end : string -> string -> int -> int -> unit
val log_async_instant : string -> string -> string -> int -> int -> unit

val log_flow_begin : string -> string -> int -> int -> unit
val log_flow_end : string -> string -> int -> int -> unit
val log_flow_instant : string -> string -> int -> int -> unit

val log_counter : string -> int -> string -> float -> unit
val log_counters : string -> int -> (string * float) list -> unit

val process_meta : int -> string -> string -> int -> unit
val thread_meta : int -> int -> string -> int -> unit

val output_json : (string -> unit) -> unit


