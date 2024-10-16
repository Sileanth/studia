# 1 "lexer.mll"
 

open Printf


# 8 "lexer.ml"

let rec __ocaml_lex_refill_buf lexbuf _buf _len _curr _last =
  if lexbuf.Lexing.lex_eof_reached then
    256, _buf, _len, _curr, _last
  else begin
    lexbuf.Lexing.lex_curr_pos <- _curr;
    lexbuf.Lexing.lex_last_pos <- _last;
    lexbuf.Lexing.refill_buff lexbuf;
    let _curr = lexbuf.Lexing.lex_curr_pos in
    let _last = lexbuf.Lexing.lex_last_pos in
    let _len = lexbuf.Lexing.lex_buffer_len in
    let _buf = lexbuf.Lexing.lex_buffer in
    if _curr < _len then
      Char.code (Bytes.unsafe_get _buf _curr), _buf, _len, (_curr + 1), _last
    else
      __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
  end

let rec __ocaml_lex_state7 lexbuf _last_action _buf _len _curr _last =
  let next_char, _buf, _len, _curr, _last =
    if _curr >= _len then
      __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
    else
      Char.code (Bytes.unsafe_get _buf _curr),
      _buf, _len, (_curr + 1), _last
  in
  begin match next_char with
    (* |'b' *)
    | 98 ->
      __ocaml_lex_state7 lexbuf _last_action _buf _len _curr _last
    (* |'a' *)
    | 97 ->
      (* *)
      lexbuf.Lexing.lex_curr_pos <- _curr;
      lexbuf.Lexing.lex_last_pos <- _last;
      1
    | _ ->
      let _curr = _last in
      lexbuf.Lexing.lex_curr_pos <- _curr;
      lexbuf.Lexing.lex_last_pos <- _last;
      _last_action
  end

and __ocaml_lex_state8 lexbuf _last_action _buf _len _curr _last =
  (* *)
  let _last = _curr in
  let _last_action = 0 in
  let next_char, _buf, _len, _curr, _last =
    if _curr >= _len then
      __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
    else
      Char.code (Bytes.unsafe_get _buf _curr),
      _buf, _len, (_curr + 1), _last
  in
  begin match next_char with
    (* |'a' *)
    | 97 ->
      let next_char, _buf, _len, _curr, _last =
        if _curr >= _len then
          __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
        else
          Char.code (Bytes.unsafe_get _buf _curr),
          _buf, _len, (_curr + 1), _last
      in
      begin match next_char with
        (* |'b' *)
        | 98 ->
          let next_char, _buf, _len, _curr, _last =
            if _curr >= _len then
              __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
            else
              Char.code (Bytes.unsafe_get _buf _curr),
              _buf, _len, (_curr + 1), _last
          in
          begin match next_char with
            (* |'a' *)
            | 97 ->
              __ocaml_lex_state8 lexbuf 0 (* = last_action *) _buf _len _curr _last
            | _ ->
              let _curr = _last in
              lexbuf.Lexing.lex_curr_pos <- _curr;
              lexbuf.Lexing.lex_last_pos <- _last;
              0 (* = last_action *)
          end
        | _ ->
          let _curr = _last in
          lexbuf.Lexing.lex_curr_pos <- _curr;
          lexbuf.Lexing.lex_last_pos <- _last;
          0 (* = last_action *)
      end
    | _ ->
      let _curr = _last in
      lexbuf.Lexing.lex_curr_pos <- _curr;
      lexbuf.Lexing.lex_last_pos <- _last;
      0 (* = last_action *)
  end


let rec reg lexbuf =
  let __ocaml_lex_result =
    let _curr = lexbuf.Lexing.lex_curr_pos in
    let _last = _curr in
    let _len = lexbuf.Lexing.lex_buffer_len in
    let _buf = lexbuf.Lexing.lex_buffer in
    let _last_action = -1 in
    lexbuf.Lexing.lex_start_pos <- _curr;
    let next_char, _buf, _len, _curr, _last =
      if _curr >= _len then
        __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
      else
        Char.code (Bytes.unsafe_get _buf _curr),
        _buf, _len, (_curr + 1), _last
    in
    begin match next_char with
      (* |'\n' *)
      | 10 ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        4
      (* |eof *)
      | 256 ->
        let _curr = _last in
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        _last_action
      (* |'a' *)
      | 97 ->
        (* *)
        let _last = _curr in
        let _last_action = 2 in
        let next_char, _buf, _len, _curr, _last =
          if _curr >= _len then
            __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
          else
            Char.code (Bytes.unsafe_get _buf _curr),
            _buf, _len, (_curr + 1), _last
        in
        begin match next_char with
          (* |'a' *)
          | 97 ->
            (* *)
            lexbuf.Lexing.lex_curr_pos <- _curr;
            lexbuf.Lexing.lex_last_pos <- _last;
            1
          (* |'b' *)
          | 98 ->
            let next_char, _buf, _len, _curr, _last =
              if _curr >= _len then
                __ocaml_lex_refill_buf lexbuf _buf _len _curr _last
              else
                Char.code (Bytes.unsafe_get _buf _curr),
                _buf, _len, (_curr + 1), _last
            in
            begin match next_char with
              (* |'b' *)
              | 98 ->
                __ocaml_lex_state7 lexbuf 2 (* = last_action *) _buf _len _curr _last
              (* |'a' *)
              | 97 ->
                __ocaml_lex_state8 lexbuf 2 (* = last_action *) _buf _len _curr _last
              | _ ->
                let _curr = _last in
                lexbuf.Lexing.lex_curr_pos <- _curr;
                lexbuf.Lexing.lex_last_pos <- _last;
                2 (* = last_action *)
            end
          | _ ->
            let _curr = _last in
            lexbuf.Lexing.lex_curr_pos <- _curr;
            lexbuf.Lexing.lex_last_pos <- _last;
            2 (* = last_action *)
        end
      (* |'b' *)
      | 98 ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        3
      | _ ->
        (* *)
        lexbuf.Lexing.lex_curr_pos <- _curr;
        lexbuf.Lexing.lex_last_pos <- _last;
        5
    end
  in
  begin
    let _curr_p = lexbuf.Lexing.lex_curr_p in
    if _curr_p != Lexing.dummy_pos then begin
      lexbuf.Lexing.lex_start_p <- _curr_p;
      lexbuf.Lexing.lex_curr_p <-
        {_curr_p with Lexing.pos_cnum =
         lexbuf.Lexing.lex_abs_pos+lexbuf.Lexing.lex_curr_pos}
    end
  end;
  match __ocaml_lex_result with
  | 0 ->
# 18 "lexer.mll"
          ( printf "(aba)+\n"; reg lexbuf)
# 208 "lexer.ml"

  | 1 ->
# 19 "lexer.mll"
                ( printf "ab*a\n"; reg lexbuf)
# 213 "lexer.ml"

  | 2 ->
# 20 "lexer.mll"
          ( printf "a\n"; reg lexbuf)
# 218 "lexer.ml"

  | 3 ->
# 21 "lexer.mll"
          ( printf "b\n"; reg lexbuf)
# 223 "lexer.ml"

  | 4 ->
# 22 "lexer.mll"
          ( reg lexbuf )
# 228 "lexer.ml"

  | 5 ->
let
# 23 "lexer.mll"
       c
# 234 "lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 23 "lexer.mll"
         ( printf "unexcpected token: %c\n" c; reg lexbuf )
# 238 "lexer.ml"

  | _ -> raise (Failure "lexing: empty token")


;;

# 26 "lexer.mll"
 
  let main () =
    let cin =
      if Array.length Sys.argv > 1
      then open_in Sys.argv.(1)
      else stdin
    in
    let lexbuf = Lexing.from_channel cin in
    reg lexbuf

  let _ = Printexc.print main ()

# 258 "lexer.ml"
