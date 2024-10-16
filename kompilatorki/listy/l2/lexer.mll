{

open Printf

}

(* rule reg = parse *)
(* | "aba"+  { printf "(aba)+\n"; } *)
(* | "a" "b"* "a"  { printf "ab*a\n"; } *)
(* | "a"     { printf "a\n"; } *)
(* | "b"     { printf "b\n"; } *)
(**)




rule reg = parse
| "aba"+  { printf "(aba)+\n"; reg lexbuf}
| "a" "b"* "a"  { printf "ab*a\n"; reg lexbuf}
| "a"     { printf "a\n"; reg lexbuf}
| "b"     { printf "b\n"; reg lexbuf}
| '\n'    { reg lexbuf }
| _ as c { printf "unexcpected token: %c\n" c; reg lexbuf }


{
  let main () =
    let cin =
      if Array.length Sys.argv > 1
      then open_in Sys.argv.(1)
      else stdin
    in
    let lexbuf = Lexing.from_channel cin in
    reg lexbuf

  let _ = Printexc.print main ()
}
