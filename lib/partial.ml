module Eff = Effect
open Eff.Deep
open EffTypes

module Partial = struct

  (* Effect constructors *)
  type _ Eff.t +=
    | SomeE : 'a -> 'a Eff.t
    | NoneE : 'a Eff.t

  (* Prettier constructors *)
  let some : 'a -> 'a = fun x -> Eff.perform (SomeE x)
  let none : unit -> 'a = fun () -> Eff.perform NoneE

  (* A handler that takes a computation with partial computaiton effects
     to an optional value. *)
  module Optional = struct
    let retc : type a. a -> a option =
      fun x -> Some x

    let effc : type a b. (a, b option) effc = function
      | SomeE x -> Some (fun k  -> continue k x)
      | NoneE   -> Some (fun _k -> None)
      | _       -> None

    let handler : ('a, 'b) handler = fun f () ->
      match_with f () {retc; effc; exnc = raise}
  end

  (* A handler that aborts a partial computation with an exception,
     and otherwise returns the value. *)
  module Abort = struct
    exception Failed

    let retc : type a. a -> a =
      fun x -> x

    let effc : type a b. (a, b) effc = function
      | SomeE x -> Some (fun k  -> continue k x)
      | NoneE   -> Some (fun _k -> raise Failed)
      | _       -> None

    let handler : ('a, 'b) handler = fun f () ->
      match_with f () {retc; effc; exnc = raise}
  end

  let or_else ~default f =
    match Optional.handler f () with
    | None   -> default
    | Some x -> x

  (* A handler that returns a default value for a partial computation
     with an exception, and otherwise returns the value. *)
  (* module Default = struct *)

  (*   let handler ~default f () = *)
  (*     match Optional.handler f () with *)
  (*     | None -> default *)
  (*     | Some x -> x *)
  (* end *)
end


let%test_module "partial computation effects" = (module struct
  open Partial

  let (/) n = function
    | 0. -> none ()
    | d  -> some (Float.div n  d)

  let head = function
    | []     -> none ()
    | x :: _ -> some x

  let tail = function
    | []      -> none ()
    | _ :: tl -> some tl

  let%test "direct style with successful operations" =
    let progn () =
      (head [4. ; 3. ; 2.]) / 2. :: tail [2. ; 3. ; 4.]
    in
    Optional.handler progn () = Some [2. ; 3. ; 4.]

  let failing_progn () =
    let z = 1. / 0. in
    let x = head [z ; 2. ; 3.] in
    let y = tail [1.] in
    x :: y

  let%test "direct style with a failing operation" =
    Optional.handler failing_progn () = None

  (* TODO There is probably a better way to do this *)
  let%test "direct style with a default?" =
    let progn () =
      let z = (fun () -> 1. / 0.) |> or_else ~default:2. in
      let x = head [z ; 2. ; 3.] in
      let y = tail [4. ; 5. ; 6.] in
      x :: y
    in
    Optional.handler progn () = Some [2. ; 5. ; 6.]

  let%test "direct style with exceptional failures" =
    try Abort.handler failing_progn () = [Float.nan]
    with Abort.Failed -> true

  let%test "composition of partial computation handler with state handler" =
    let module IntState = State.State (Int) in
    let open IntState in
    let progn () =
      let x = head [ get ()  ; 2 ; 3 ] in
      let () = set 100 in
      let y = tail [ 4 ; get () ; 6 ] in
      let () = set 42 in
      x :: y
    in
    let init = 0 in
    let res, final = (IntState.Update.handler @@ Optional.handler progn) () init in
    res = Some [0 ; 100; 6] && final = 42
end)
