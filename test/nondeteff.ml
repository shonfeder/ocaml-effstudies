open Effstudies

let () = match Sys.argv.(1) with
  | "choose-true"  -> Nondet.run_choose_true ()
  | "choose-false" -> Nondet.run_choose_false ()
  (* NOTE: Cannot run, because OCaml 5 does not allow multi-shot continuations  *)
  (* | "choose-max"   -> Nondet.run_choose_max () *)
  | _              -> failwith "invalid command"
