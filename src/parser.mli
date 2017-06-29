type error

val error_msg : error -> string
val parse : string -> (Utils.BFInstr.bf_instr * Utils.FilePos.pos) Queue.t * error Queue.t
