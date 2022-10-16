(** A computation of a value of type 'a is modeled as a thunk *)
type 'a comp = unit -> 'a

(** A handler takes one computation, and returns a new computation that will
    handle some of the effects that may be performed *)
type ('a, 'b) handler = 'a comp -> 'b comp

(** An effect case for an effect handler takes an effect of of type 'a to
    some function from a continuation from 'a to 'b to the value 'b, or nothing. *)
type ('a, 'b) effc = 'a Effect.t -> (('a, 'b) Effect.Deep.continuation -> 'b) option
