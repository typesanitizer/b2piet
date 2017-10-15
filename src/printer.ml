open Batteries

(* Most of the documentation for `camlimages` is in its file `images.mli`. *)

let colour_to_rgb c =
  let x = Utils.Piet.colour_to_hex c in
  Images.{r = x / (256 * 256); g = x / 256 mod 256; b = x mod 256;}

(* Print 2D codel array to file. *)
let save_picture fname codel_dim (clr_a2d, w, h) =
  let new_w = w * codel_dim in
  let new_h = h * codel_dim in
  let img = Rgb24.create new_w new_h in
  Array.(iteri (fun y ->
      iteri (fun x ->
          Rgb24.set img (x * codel_dim) (y * codel_dim) % colour_to_rgb)))
    clr_a2d;
  if codel_dim > 1 then
    for i = 0 to w - 1 do
      for j = 0 to h - 1 do
        for m = 1 to codel_dim - 1 do
          Rgb24.blit
            img (i * codel_dim) (j * codel_dim)
            img (i * codel_dim + m) (j * codel_dim)
            1 1;
        done;
        for n = 1 to codel_dim - 1 do
          Rgb24.blit
            img (i * codel_dim) (j * codel_dim)
            img (i * codel_dim) (j * codel_dim + n)
            codel_dim 1
        done
      done
    done;
  Images.save fname None [] (Images.Rgb24 img)
