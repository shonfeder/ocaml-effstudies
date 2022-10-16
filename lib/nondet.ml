module Eff = Effect
let continue = Eff.Deep.continue

open EffTypes

type _ Eff.t +=
  (* Decide is an effect that non-deterministically yields a boolean *)
  | Decide : bool Eff.t

let decide () = Eff.perform Decide

let choose: 'a -> 'a -> 'a = fun x y ->
  let b = decide () in
  if b then x else y

let const_handler (b : bool) : ('a, 'b) handler =
  let effc : type a b. (a, b) effc = function
    | Decide -> Some (fun k -> Eff.Deep.continue k b)
    | _      -> None
  in
  fun f () ->
  Eff.Deep.try_with f () { effc }

let max_handler : ('a, 'b) handler =
  let effc : type a. (a, int) effc = function
    | Decide -> Some (fun k ->
        let xt = continue k true in
        let xf = continue k false in
        Int.max xt xf)
    | _ -> None
  in
  fun f () ->
    Eff.Deep.try_with f () { effc }

let choose_diff () =
  let x1 = choose 3 5 in
  let x2 = choose 3 10 in
  x1 - x2

let run_choose_true () =
  ()
  |> const_handler true choose_diff
  |> string_of_int
  |> print_endline

let run_choose_false () =
  ()
  |> const_handler false choose_diff
  |> string_of_int
  |> print_endline

let run_choose_max () =
  ()
  |> max_handler choose_diff
  |> string_of_int
  |> print_endline
