
let unique_vars = Flow.Tag.create
  "unique_vars"

let unique_type_vars = Flow.Tag.create
  "unique_type_vars"

let eval = Flow.Tag.create
  ~cmd_flag:  "-eval"
  ~cmd_descr: " Run a program"
  "eval"
