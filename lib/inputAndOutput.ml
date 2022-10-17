module Eff = Effect
open Eff
open Eff.Deep

module E = struct
  type _ Eff.t +=
    | Print : string -> unit Eff.t
    | Read : string Eff.t

  let print s = perform (Print s)
  let read () = perform Read
end

module Handler = struct
  open E
      open EffTypes

  (** The base handler for the print effect, finally delegates printing to an IO operation *)
  let print : ('a, 'b) handler =
    let effc : type a b. (a, b) effc = function
      | Print s -> Some (fun k -> print_endline s; continue k ())
      | _       -> None
    in
    fun f () -> try_with f () { effc }

  (** The base handler for the read effect, finally delegates reading to an IO operation *)
  let read : ('a, 'b) handler =
    let effc : type a b. (a, b) effc = function
        | Read -> Some (fun k -> let i = read_line () in continue k i)
        | _    -> None
    in
    fun f () -> try_with f () { effc }

  (** Standard handling of IO *)
  let stdio : ('a, 'b) handler = fun f -> print @@ read @@ f

  (** Intercepts any read effect, suppling a constant value *)
  let read_constant (const : string) : ('a, 'b) handler =
    let effc : type a b. (a, b) effc = function
      | Read -> Some (fun (k: (a, _) continuation) -> continue k const)
      | _    -> None
    in
    fun f () -> try_with f () { effc }

  (** Reverse any print effects *)
  let print_reverse : ('a, 'b) handler =
    let effc : type a. (a, unit) effc = function
      (* To reverse the print effects, we simply return the continuation *before*
         we repeat the print effect *)
      | Print s -> Some (fun k -> continue k (); E.print s)
      | _       -> None
    in
    fun f () -> try_with f () { effc }

  (** Collect any output, instead of printing it *)
  let collect_output : ('a, 'a * string list) handler =
    (* Pure values are returned with an empty list of outputs *)
    let retc x = (x, []) in
    let effc : type a b. (a, b * string list) effc =  function
      (* Intercept the print effect *)
      | Print s -> Some (fun k ->
          (* get the value of the continuation, along with its accumulated output *)
          let (x, acc) = continue k () in
          (* add the output from this print effect to the subsequently accumulated outputs *)
          (x, s :: acc))
      | _ -> None
    in
    fun f () -> match_with f () { retc; effc; exnc = raise }

  (** [collect_output] but using parameter-passing *)
  let collect_output' : ('a, string list -> ('a * string list)) handler =
    let retc x = fun acc -> (x, acc) in
    let effc : type a b. (a, string list -> b * string list) effc = function
      | Print s -> Some (fun k -> fun acc -> (continue k ()) (s :: acc))
      | _ -> None
    in
    fun f () -> match_with f () { retc; effc; exnc = raise }
end

open E

let print_full_name () =
  print "What is your first name?";
  let first_name = read () in
  print "What is your last name?";
  let last_name = read () in
  print (first_name ^ " " ^ last_name)

let run_read = Handler.stdio @@ print_full_name
let run_read_constant = Handler.print @@ Handler.read_constant "FOO" @@ print_full_name

let echo_3 () =
  let a = read () in
  print a;
  let b = read () in
  print b;
  let c = read () in
  print c

let run_echo = Handler.stdio echo_3
let run_echo_reverse = Handler.stdio @@ Handler.print_reverse @@ echo_3

let run_collect_echo () =
  ()
  |> Handler.stdio @@ Handler.collect_output @@ Handler.print_reverse @@ echo_3
  |> snd
  |> String.concat ", "
  |> print_endline

(** Run output collecter using the parameter passing style *)
let run_collect_echo' () =
  let ((), collected) =
   (Handler.stdio @@ Handler.collect_output' @@ Handler.print_reverse @@
    echo_3) () []
  in
  String.concat ", " collected |> print_endline
