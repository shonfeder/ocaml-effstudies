module Eff = struct
  open Effect
  open Effect.Deep

  module Parser = struct
    type t = string list -> string list

    let run : t -> string list -> string list = fun p args -> p args
  end

  type _ Effect.t += Parse : Parser.t -> Parser.t t

  type 'a status =
    | Parsed of 'a
    | Registered of
        { parser : Parser.t
        ; cont : (Parser.t, 'a status) continuation
        }

  (** [f] is a computation that can perform a parse effect and return a
      resulting value of type ['a] *)
  let register : (unit -> 'a) -> unit -> 'a status =
   fun f () ->
    match_with
      f
      ()
      { retc = (fun v -> Parsed v)
      ; exnc = raise
      ; effc =
          (fun (type a) (eff : a t) ->
            match eff with
            | Parse parser ->
                Some
                  (fun (cont : (a, _) continuation) ->
                    Registered { parser; cont })
            | _            -> None)
      }

  let rec parse_both a b =
    match (a (), b ()) with
    | Parsed a', Parsed b' -> (a', b')
    | Registered r1, Registered r2 ->
        let _TODO =
          Sys.argv
          |> Array.to_list
          |> Parser.run r1.parser
          |> Parser.run r2.parser
        in
        (* TODO What to do here? *)
        parse_both
          (fun () -> continue r1.cont r1.parser)
          (fun () -> continue r2.cont r2.parser)
    | _ -> failwith "TODO Not sure what this means :)"

  (* TODO replace ref with tuple returning value *)
  let parse_flag flags r : Parser.t =
   fun args ->
    match List.partition (fun x -> List.mem x flags) args with
    | [], rest     ->
        r := Some false;
        rest
    | _ :: _, rest ->
        r := Some true;
        rest

  (* TODO open bug for perform assigned to anonymous var raising bug *)

  let foo () =
    let r = ref None in
    let i = perform (Parse (parse_flag [ "--foo" ] r)) in
    ignore i;
    Option.get !r

  let bar () =
    let r = ref None in
    let i = perform (Parse (parse_flag [ "--bar" ] r)) in
    ignore i;
    Option.get !r

  let main () =
    let foo, bar = parse_both (register foo) (register bar) in
    Printf.printf "FOO: %b; BAR: %b\n" foo bar

  (* register recieves a parser and a continuation *)
  (* continuations are cached in a map or whatever *)
  (* when the `Parse` efect is performed, execute all continuations *)
  (* the continuation performs a `Value` effect, which receives back the value, they can communicate via a ref, if needed. *)

  (* let main () = *)
  (*   let n = *)
  (*     try_with *)
  (*       comp1 *)
  (*       () *)
  (*       { effc = *)
  (*           (fun (type a) (eff : a t) -> *)
  (*             match eff with *)
  (*             | Xchg n -> *)
  (*                 print_endline "xcgh"; *)
  (*                 Some (fun (k : (a, _) continuation) -> continue k (n + 1)) *)
  (*             | Inc2 n -> *)
  (*                 print_endline "inc2"; *)
  (*               Some (fun k -> continue k (n + 2)) *)
  (*             | _      -> None) *)
  (*       } *)
  (*   in *)
  (*   Printf.printf "OUT: %i\n" n *)
end

module Threads = struct
  open Effect
  open Effect.Deep

  type task = unit -> unit

  type _ Effect.t +=
    | Fork : task -> unit Effect.t
    | Yield : unit Effect.t
    | Xchg : int -> int t

  let fork f = perform (Fork f)
  let yield () = perform Yield
  let xchg v = perform (Xchg v)

  let run : task -> unit =
   fun main ->
    let exchanger = ref None in
    let run_q = Queue.create () in
    let enqueue k v =
      let task () = continue k v in
      Queue.push task run_q
    in
    let dequeue () =
      if Queue.is_empty run_q then begin
        match !exchanger with
        | None        -> ()
        | Some (_, k) ->
            exchanger := None;
            discontinue k (Failure "dangling exchange value")
      end else
        let task = Queue.pop run_q in
        task ()
    in
    let rec spawn : task -> unit =
     fun f ->
      match_with
        f
        ()
        { retc = dequeue
        ; exnc =
            (fun e ->
              print_endline (Printexc.to_string e);
              dequeue ())
        ; effc =
            (fun (type a) (eff : a t) ->
              match eff with
              | Yield  ->
                  Option.some @@ fun (k : (a, unit) continuation) ->
                  enqueue k ();
                  dequeue ()
              | Fork f ->
                  Option.some @@ fun (k : (a, unit) continuation) ->
                  enqueue k ();
                  spawn f
              | Xchg n -> (
                  Option.some @@ fun (k : (int, unit) continuation) ->
                  match !exchanger with
                  | Some (n', k') ->
                      exchanger := None;
                      enqueue k' n;
                      continue k n'
                  | None          ->
                      exchanger := Some (n, k);
                      dequeue ())
              | _      -> None)
        }
    in
    spawn main

  open Printf

  let main () =
    run (fun () ->
        fork (fun () ->
            printf "t1: sending 0\n";
            xchg 0 |> printf "t1: received %n\n");
        fork (fun () ->
            printf "t2: sending 1\n";
            xchg 1 |> printf "t2: received %n\n"))
end

module Invert = struct
  open Effect
  open Effect.Deep

  let invert (type a) : iter:((a -> unit) -> unit) -> a Seq.t =
   fun ~iter ->
    let module M = struct
      type _ Effect.t += Yield : a -> unit Effect.t
    end in
    let yield v = perform (M.Yield v) in
    fun () ->
      match_with
        iter
        yield
        { retc = (fun _ -> Seq.Nil)
        ; exnc = raise
        ; effc =
            (fun (type b) (eff : b Effect.t) ->
              match eff with
              | M.Yield v ->
                  Some
                    (fun (k : (b, _) continuation) -> Seq.Cons (v, continue k))
              | _         -> None)
        }
end

module EffParser = struct
  open Effect
  open Effect.Deep

  type parser_kind =
    | Flg
    | Opt
    | Arg

  type 'a parser = string list -> 'a * string list

  type 'a arg_parser =
    { kind : parser_kind
    ; parser : 'a parser
    }

  type _ Effect.t +=
    | Parse : 'a arg_parser -> 'a Effect.t
    | Value : 'a -> 'a Effect.t

  module Hlist = struct
    type ('hd, 'tl) t =
      | Nil : ('hd, 'tl) t
      | Cons : 'hd * ('tl, 'a) t -> ('hd, 'tl * 'a) t

    let empty = Nil
    let cons : type a b c. a -> (b, c) t -> (a, b * c) t = fun x q -> Cons (x, q)

    let snoc : type a b c. (a, b * c) t -> a * (b, c) t = function
      | Nil           -> failwith "TODO empty"
      | Cons (hd, tl) -> (hd, tl)
  end

  let parse (type a) : (unit -> a) -> a =
   fun parser ->
    (* let args = ref (Sys.argv |> Array.to_list) in *)
    (* let flg_parsers = Queue.create () in *)
    (* let arg_parsers = Queue.create () in *)
    (* let pos_parsers = Queue.create () in *)
    let rec aux :
        type f fs o os ar ars.
        (f, fs) Hlist.t -> (o, os) Hlist.t -> (ar, ars) Hlist.t -> a =
     fun flgs opts args ->
      match_with
        parser
        ()
        { retc = (fun v -> print_endline "DO STUFF"; v)
        ; exnc = raise
        ; effc =
            (fun (type b) (eff : b Effect.t) ->
              match eff with
              | Value v                ->
                  Some (fun (k : (b, _) continuation) -> continue k v)
              | Parse { kind; parser } ->
                  Some
                    (fun (k : (b, _) continuation) ->
                      match kind with
                      | Flg -> aux (Hlist.cons (parser, k) flgs) opts args
                      | Opt -> aux flgs (Hlist.cons (parser, k) opts) args
                      | Arg -> aux flgs opts (Hlist.cons (parser, k) args))
              | _                      -> None)
        }
    in
    aux Hlist.empty Hlist.empty Hlist.empty

  type config =
    { foo : int
    ; bar : string
    ; baz : bool
    }

  let flag flags : bool =
    let parser : bool parser =
     fun args ->
      match List.partition (fun x -> List.mem x flags) args with
      | [], args'     -> (false, args')
      | _ :: _, args' -> (true, args')
    in
    perform (Parse { kind = Flg; parser })

  let is_flag = String.starts_with ~prefix:"-"

  let opt (type a) ~flags ~conv ~(default : a) : a =
    let parser : a parser =
     fun args ->
      let rec aux args acc =
        match args with
        | [] -> (default, [])
        | f :: arg :: rest when List.mem f flags -> (
            if is_flag arg then
              failwith "TODO: missing argument err"
            else
              match conv arg with
              | None   -> failwith "TODO: invalid conv err"
              | Some v -> (v, List.rev_append acc rest))
        | [ f ] when List.mem f flags -> failwith "TODO: missing argument err"
        | x :: rest -> aux rest (x :: acc)
      in
      aux args []
    in
    perform (Parse { kind = Opt; parser })

  let main () =
    let parser () =
      { foo = opt ~flags:[ "-f"; "--foo" ] ~conv:int_of_string_opt ~default:700
      ; bar = perform (Value "bar")
      ; baz = flag [ "-b"; "--baz" ]
      }
    in
    let c = parse parser in
    Printf.printf "CmdEff: %i\nbar: %s\nbaz: %b" c.foo c.bar c.baz
end

(* module EfP2 = struct *)
(*   module Eff = Effect *)
(*   open Eff.Deep *)

(*   module ArgParser = struct *)
(*     type kind = *)
(*       | Flg *)
(*       | Opt *)
(*       | Arg *)

(*     type 'a t = *)
(*       { kind : kind *)
(*       ; flags : string list *)
(*       ; set : string -> unit *)
(*       ; cell : 'a ref *)
(*       } *)
(*   end *)

(*   type _ Eff.t += Parse : 'a ArgParser.t -> 'a Eff.t *)

(*   let flg flags : bool = *)
(*     let cell = ref false in *)
(*     let p = *)
(*       ArgParser.{ kind = Flg; flags; set = (fun _ -> cell := true); cell } *)
(*     in *)
(*     Eff.perform (Parse p) *)

(*   let parse' _ = () *)

(*   let parse (type a) : (unit -> a) -> a = *)
(*    fun parser -> *)
(*     match_with *)
(*       parser *)
(*       () *)
(*       { retc = (fun v -> parse' Sys.argv; v) *)
(*       ; exnc = raise *)
(*       ; effc = *)
(*           (fun (type b) (eff : b Eff.t) -> *)
(*             match eff with *)
(*             | Parse p -> Some (fun (k : (b, _) continuation) -> ()) *)
(*             | _       -> None) *)
(*       } *)
(* end *)
