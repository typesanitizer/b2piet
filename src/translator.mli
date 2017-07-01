type ir
type runtime_err

(**
   Takes a list of instructions and an argument specifying if condensation
   ought to be performed. If it is true, `++` would be translated to
   `add two, mod 256` rather than `add 1, mod 256, add 1, mod 256`. Similarly,
   a sequence of 256 decrements would be translated to a no-op.
*)
val translate :
  (Utils.BFInstr.bf_instr * Utils.FilePos.pos) list ->
  bool -> bool -> int -> bool ->
  (ir list * runtime_err option)
val print_ast : ir list -> unit list
