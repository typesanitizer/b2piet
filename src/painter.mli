(**
   Type to be used by `Printer` module for saving to an image.
*)
type picture = Utils.Piet.colour BatVect.t BatVect.t * int * int

type push_style = Literal
                | Fast of (int * int * Utils.FastPush.push_op list) list
type draw_style = Linear | Tableau

val paint : push_style -> draw_style -> Utils.PietIR.ir list -> picture

val interpret : Utils.PietIR.ir list -> int BatVect.t

val tableau_show :
  (int * int * Utils.FastPush.push_op list) list ->
  Utils.PietIR.ir list -> string

val mesh_show :
  (int * int * Utils.FastPush.push_op list) list ->
  Utils.PietIR.ir list -> string
