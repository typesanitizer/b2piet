type runtime_err
type interpret_err

(**
   Interprets Brainfuck instructions and gives maximum value of dp and errors.
   The result may be incorrect if the first argument contains input
   instructions.
*)
val interpret :
  (Utils.BFInstr.t * Utils.FilePos.t) list ->
  int * interpret_err option

(**
   Same as interpret but all gives an output string as well.
   The result may be incorrect if the first argument contains input
   instructions.
*)
val interpret_woutput :
  (Utils.BFInstr.t * Utils.FilePos.t) list ->
  (int * string * interpret_err option) Lwt.t

(**
   Takes a list of instructions and an argument specifying if condensation
   ought to be performed. If it is true, `++` would be translated to
   `add two` rather than `add 1, add 1`. Similarly, a sequence of 256
   decrements would be translated to a no-op.
*)
val translate :
  ?condense:bool ->
  ?loops_present:bool ->
  ?stack_size:int ->
  ?shrink_rolls:bool ->
  ?modulo:bool ->
  (Utils.BFInstr.t * Utils.FilePos.t) list ->
  (Utils.PietIR.ir list * runtime_err option)
