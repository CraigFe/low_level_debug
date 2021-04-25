(* Useful debugging functions working at a low level. *)

type never_returns = |

external stop_upon_sigbus : unit -> unit = "low_level_debug_stop_upon_sigbus"
external stop_upon_sigsegv : unit -> unit = "low_level_debug_stop_upon_sigsegv"
external stop_upon_sigpipe : unit -> unit = "low_level_debug_stop_upon_sigpipe"
external stop_upon_exit : unit -> unit = "low_level_debug_stop_upon_exit"
external stop_me_now : unit -> unit = "low_level_debug_stop_me_now"
external int_as_pointer : int -> 'a = "%int_as_pointer"

let segfault_me_now () =
  let string_that_should_be_easy_to_detect =
    "The major difference between a thing that might go wrong and a thing that \
     cannot possibly go wrong is that when a thing that cannot possibly go \
     wrong goes wrong it usually turns out to be impossible to get at or \
     repair."
  in
  (* The use of [int_as_pointer] circumvents Flambda saying that the value at
     address zero is not mutable. *)
  Obj.set_field (int_as_pointer 0) 0
    (Obj.repr string_that_should_be_easy_to_detect)

external start_canary_thread_internal :
  max_wait:float -> check_interval:int -> never_returns = "start_canary"

let start_canary_thread =
  let started = ref false in
  let lock = Mutex.create () in
  fun ~max_wait ~check_interval ->
    Mutex.lock lock;
    if !started then
      failwith "canary thread already started, one allowed per process";
    started := true;
    Mutex.unlock lock;
    if check_interval >= 1. then invalid_arg "check_interval must be < 1s";
    let check_interval = Float.to_int (check_interval *. 1_000_000.) in
    let (_ : Thread.t) =
      Thread.create
        (fun () -> start_canary_thread_internal ~max_wait ~check_interval)
        ()
    in
    ()

(* It seems that veneers like these are needed so that the functions are
   correctly exported in the object files. *)
let stop_upon_sigbus () = stop_upon_sigbus ()
let stop_upon_sigsegv () = stop_upon_sigsegv ()
let stop_upon_sigpipe () = stop_upon_sigpipe ()
let stop_upon_exit () = stop_upon_exit ()
let stop_me_now () = stop_me_now ()

let rec pp_obj : Format.formatter -> Obj.t -> unit =
 fun ppf x ->
  if Obj.is_int x then Fmt.int ppf (Obj.obj x : int)
  else if Obj.is_block x then
    let tag = Obj.tag x in
    (* no-scan tags *)
    if tag = Obj.string_tag then Fmt.Dump.string ppf (Obj.obj x : string)
    else if tag = Obj.double_tag then Fmt.float ppf (Obj.obj x : float)
    else if tag = Obj.double_array_tag then
      Fmt.(Dump.array float) ppf (Obj.obj x : float array)
    else if tag = Obj.abstract_tag then Fmt.string ppf "<abstract>"
    else if tag = Obj.custom_tag then Fmt.string ppf "<custom>"
    else if tag = Obj.int_tag then Fmt.string ppf "<int>"
    else if tag = Obj.out_of_heap_tag then Fmt.string ppf "<out_of_heap>"
    else if tag = Obj.unaligned_tag then Fmt.string ppf "<unaligned>"
      (* scannable tags that we don't look further into *)
    else if tag = Obj.closure_tag then Fmt.string ppf "<function>"
    else if tag = Obj.object_tag then Fmt.string ppf "<object>"
    else if tag = Obj.lazy_tag then Fmt.string ppf "<lazy>"
    else if tag = Obj.infix_tag then Fmt.string ppf "<infix>"
    else if tag = Obj.forward_tag then Fmt.string ppf "<forward>"
      (* Any other scannable tag we dig into recursively *)
    else if tag >= 0 && tag < Obj.no_scan_tag then
      let size = Obj.size x in
      Fmt.Dump.list pp_obj ppf (List.init size (fun i -> Obj.field x i))
    else Fmt.string ppf "<unknown>"
  else Fmt.string ppf "<external pointer>"

let pp_obj ppf x = pp_obj ppf (Obj.repr x)
let obj_to_string x = Fmt.to_to_string pp_obj x
