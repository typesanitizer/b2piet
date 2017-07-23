(**
   Type to be used by `Printer` module for saving to an image.
*)
type picture = Utils.Piet.colour BatVect.t BatVect.t * int * int

(**
   Make the Piet code in a linear format. Typically the result will be a short
   but very wide image.
*)
val paint_linear : Utils.PietIR.ir list -> picture
(**
   Identical to `paint_linear` but fast pushes are used so image size will
   typically be smaller.
*)
val paint_linear_fast : stack_size:int -> Utils.PietIR.ir list -> picture
