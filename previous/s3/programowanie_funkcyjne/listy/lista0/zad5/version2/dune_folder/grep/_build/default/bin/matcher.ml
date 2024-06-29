
type t = Str.regexp

let create = Str.regexp

let match_line line pat = Str.string_match pat line 0
