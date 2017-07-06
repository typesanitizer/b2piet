open Batteries
open Painter

(* Most documentation for `camlimages` is inside its `images.mli` file. *)
module I = Images
module P = Utils.Piet
module V = BatVect

(* type I.rgb = Rgb24.elt *)
let colour_to_rgb : P.colour -> I.rgb = fun c ->
  let x = P.colour_to_hex c in
  {I.r = x/(256*256); I.g = x/256 mod 256; I.b = x mod 256;}

(* Print 2D codel array to file. *)
let save_picture (clr_a2d, w, h) fname codel_dim =
  let new_w = w * codel_dim in
  let new_h = h * codel_dim in
  let img = Rgb24.create new_w new_h in
  V.iteri (fun y ->
      V.iteri (fun x ->
          Rgb24.set img (x*codel_dim) (y*codel_dim) % colour_to_rgb))
    clr_a2d;
  if codel_dim > 1 then
    List.iter (fun i ->
        List.iter (fun j ->
            List.iter (fun m ->
                Rgb24.blit
                  img (i*codel_dim) (j*codel_dim)
                  img (i*codel_dim + m) (j*codel_dim)
                  1 1
              ) %  List.range 1 `To @@ codel_dim-1;
            List.iter (fun n ->
                Rgb24.blit
                  img (i*codel_dim) (j*codel_dim)
                  img (i*codel_dim) (j*codel_dim + n)
                  codel_dim 1
              ) % List.range 1 `To @@ codel_dim-1)
            % List.range 0 `To @@ h-1)
    % List.range 0 `To @@ w-1;
  Images.save fname None [] (I.Rgb24 img)
