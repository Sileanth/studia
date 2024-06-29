(** Abstract syntax tree of a parsed program *)

(** Node of AST, that contains additional information about the location. *)
type 'a node = {
  data      : 'a;
  start_pos : Lexing.position;
  end_pos   : Lexing.position
}

(** Variables are represented as strings *)
type var = string

(** Expressions. Each node stores additional information about the location
 * in the source, just to be able to produce better error messages. Note that
 * we do not distinguish between expressions and values: this distinction is
 * important for the semantics, but not for the programmer. Probably you will
 * need some type for representing values, but it should be defined in the
 * evaluator. *)
type expr = expr_data node
and expr_data =
  | EUnit
  | EBool   of bool
  | ENum    of int
  | EVar    of var
  | EFn     of var * expr
  | EFix    of var * var * expr
  | EApp    of expr * expr
  | EPair   of expr * expr
  | EFst    of expr
  | ESnd    of expr
  | EInl    of expr
  | EInr    of expr
  | ECase   of expr * clause * clause
  | EIf     of expr * expr * expr
  | ESeq    of expr * expr
  | EAbsurd of expr

and clause = var * expr

(** Complete programs. They are just expressions in our case *)
type program = expr
