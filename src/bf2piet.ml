open Utils.FilePos
open Parser

(* let _ = ">\n<+" |> parse |> fst |> Queue.enum |> snd |> pos_to_str |> print_endline *)
let _ = "++][[[]][[]][[]]]\n  [[]\n[][["
        |> parse
        |> snd
        |> BatQueue.enum
        |> (fun x -> BatEnum.map error_msg x)
        |> (fun x -> BatEnum.map print_string x)
        |> BatEnum.force
let _ = print_endline @@ pos_to_str @@ make_pos 10 100 150
