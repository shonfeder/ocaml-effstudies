open Effstudies.InputAndOutput

let () =
  match Sys.argv.(1) with
  | "read"          -> run_read ()
  | "read-constant" -> run_read_constant ()
  | "echo"          -> run_echo ()
  | "echo-reverse"  -> run_echo_reverse ()
  | "echo-collect"  -> run_collect_echo ()
  | "echo-collectp" -> run_collect_echo' ()
  | _               -> failwith "invalid command"
