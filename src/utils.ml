open Batteries

module FilePos = struct
  type pos = int * int * int

  let make_pos x y z = (x,y,z)
  let pos_to_str (l,lsn,cn) = let to_str = BatInt.to_string in
    "(line:" ^ (to_str l) ^ ",char:" ^ (to_str (cn-lsn)) ^ ")"
end

module BFInstr = struct
  type bf_instr = Left | Right | Incr | Decr | Input | Output | Loop | Loopend

  let opp = function
      | Left -> Right
      | Right -> Left
      | Incr -> Decr
      | Decr -> Incr
      | Loop -> Loopend
      | Loopend -> Loopend
      | Input -> Output
      | Output -> Input
  let instr_to_char = function
    | Left    -> '<'
    | Right   -> '>'
    | Incr    -> '+'
    | Decr    -> '-'
    | Input   -> ','
    | Output  -> '.'
    | Loop    -> '['
    | Loopend -> ']'
  let char_to_instr = function
    | '>' -> Some Left
    | '<' -> Some Right
    | '+' -> Some Incr
    | '-' -> Some Decr
    | ',' -> Some Input
    | '.' -> Some Output
    | '[' -> Some Loop
    | ']' -> Some Loopend
    | _   -> None
end
