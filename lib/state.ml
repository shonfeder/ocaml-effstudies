(* We'll load up some imports *)

(** Adapting from

   - https://github.com/ocamllabs/ocaml-effects-tutorial#2-effectful-computations-in-a-pure-setting
   - www.eff-lang.org/handlers-tutorial.pdf pp. 8-9

   *)

module Eff = Effect
open Eff.Deep
open EffTypes


type ('a, 't) state_handler = ('a, 't -> ('a * 't)) handler

module type STATE = sig
  type t

  val set : t -> unit
  val get : unit -> t

  module Update : sig
    val handler : ('a, t) state_handler
  end

  module Trace : sig
    val handler : ('a, t list) state_handler
  end

end


module State (S : sig type t end) : STATE with type t = S.t = struct
  type t = S.t

  type _ Eff.t +=
    | Get : t Eff.t
    | Set : t -> unit Eff.t

  let set : t -> unit = fun st -> Eff.perform (Set st)
  let get : unit -> t = fun () -> Eff.perform Get

  module Update  = struct
    let retc : type a. a -> t -> (a * t) =
      fun x st -> (x, st)

    let effc : type a b. (a, t -> b * t) effc = function
      | Get    -> Some (fun k st -> continue k st st)
      | Set st -> Some (fun k _  -> continue k () st)
      | _      -> None

    let handler : ('a, t -> ('a * t)) handler = fun f () ->
      match_with f () {retc; effc; exnc = raise}
  end

  module Trace = struct
    let retc : type a. a -> t list -> (a * t list) =
      fun x trace -> (x, List.rev trace)

    let effc : type a b. (a, t list -> (b * t list)) effc = function
      | Set st -> Some (fun k trace ->
          (* Propage the update to additional handlers the  *)
          set st;
          (* Resume the continuation, and add the state update to the trace )*)
          continue k () (st :: trace))
      | _      -> None

    let handler = fun f () ->
      match_with f () {retc; effc; exnc = raise}
  end
end

let%test_module "integer state" = (module struct
  module IntState = State(Int)

  let incr () =
    let open IntState in
    let x = get () in
    set (x + 1)

  let decr () =
    let open IntState in
    let x = get () in
    set (x - 1)

  let incr_twice () =
    incr ();
    incr ()

  let decr_twice () =
    decr ();
    decr ()

  let%test "incrementing a counter" =
    let init = 0 in
    let ((), end_state) = init |> IntState.Update.handler incr_twice () in
    end_state = init + 2

  let%test "incrementing and decrementing a counter" =
    let init = 0 in
    let ((), end_state) =
      init
      |> IntState.Update.handler (fun () -> incr_twice () |> decr_twice) ()
    in
    init = end_state

  let%test "collecting a trace of counter states" =
    let init = 0 in
    let init_trace = [init] in
    let program () =
      incr_twice ();
      decr_twice ()
    in
    (* Here we compose the `Update.handler` with the `Trace.handler`.
       The `Trace.handler` must be the inner handler, or else the
       `Update.handler` will consume the update without propagating it. *)
    let (((), trace), end_state) =
      (IntState.Update.handler @@ fun () -> IntState.Trace.handler program () init_trace) () init
    in
    init = end_state && trace = [0; 1; 2; 1; 0]

end)

let%test_module "record state" = (module struct
  module Vars = struct
    type t = {x : int; y : string}
  end
  module RecState = State (Vars)

  let incr () =
    let open RecState in
    let s = get () in
    set {s with x = s.x + 1}

  let concat str =
    let open RecState in
    let s = get () in
    set {s with y = s.y ^ str}

  let%test "collecting a trace of state updates states" =
    let init : Vars.t = {x = 0; y = "a"} in
    let init_trace = [init] in
    let program () =
      incr ();
      concat "b"
    in
    (* Here we compose the `Update.handler` with the `Trace.handler`.
       The `Trace.handler` must be the inner handler, or else the
       `Update.handler` will consume the update without propagating it. *)
    let (((), trace), _) =
      (RecState.Update.handler @@ fun () -> RecState.Trace.handler program () init_trace) () init
    in
    trace = [ {x = 0; y = "a"}
            ; {x = 1; y = "a"}
            ; {x = 1; y = "ab"}
            ]
end)
