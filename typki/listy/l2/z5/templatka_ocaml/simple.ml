(** The main module of a inerpreter *)

let pipeline fname =
  fname
  |> Parser.parse_file
  |> Eval.eval_program

let _ =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s FILE\n" Sys.argv.(0)
  else
    try pipeline Sys.argv.(1) with
    | Utils.Fatal_error -> exit 1
