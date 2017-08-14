open Batteries
let bfprog_gen = QCheck.Gen.(
    let linear_codegen =
      string_size (int_range 1 4)
        ~gen:(oneofl ['+'; '-'; '<'; '>'; '.'; ',']) in
    sized @@ fix (fun self -> function
        | 0 -> linear_codegen
        | n -> frequency
                 [4, map2 (^) linear_codegen @@ self (n/2);
                  1, map (fun s -> String.concat "[" s ^ "]")
                    (list_size (int_range 1 4) @@ self (n/2));]
      )
  )

let parse_opt = Parser.parse %> fst %> Optimiser.optimise

let all_but_print meta str =
  let bfinstr = fst (parse_opt str) in
  let stack_size = 8 in
  let (ir_l, _) = Translator.translate ~stack_size bfinstr in
  let (_, fpl) = Utils.MetaJson.get_fast_push_table stack_size meta in
  Painter.(paint (Fast fpl) Linear ir_l)

let domains_fit_correctly meta str =
  let bfinstr = fst (parse_opt str) in
  let stack_size = 8 in
  let (ir_l, _) = Translator.translate ~stack_size bfinstr in
  let (_, fpl) = Utils.MetaJson.get_fast_push_table stack_size meta in
  let domains = Painter.Test.domains fpl ir_l in
  let w = Array.length domains.(0) in
  let h = Array.length domains in
  let wh_a = Array.mapi (fun iy a ->
      Array.mapi (fun ix _ -> Painter.Test.get_wh domains iy ix) a)
      domains in
  let row_sums =
    Array.(to_list @@ map (fold_left (fun a -> fst %> (+) a) 0) wh_a) in
  let col_sums = List.(map (fun ix ->
      fold_left (fun a iy ->
          wh_a.(iy).(ix) |> snd |> (+) a) 0 @@ range 0 `To (h - 1)
    ) @@ range 0 `To (w - 1)) in
  row_sums = List.make h w && col_sums = List.make w h

let optimisation_is_stationary =
  let f s =
    let (o1, _) = parse_opt s in
    let (o2, _) = Optimiser.optimise o1 in
    o1 = o2 in
  QCheck.(Test.make
            ~count:100
            ~name:"optimisation_is_stationary"
            (make bfprog_gen) f)

let full meta str =
  let bfinstr = fst (parse_opt str) in
  let (stack_size, _, bfout_str) = Translator.interpret_woutput bfinstr in
  let (ir_l, _) = Translator.translate ~stack_size bfinstr in
  let (_, fpl) = Utils.MetaJson.get_fast_push_table ~stack_size meta in
  Painter.(paint (Fast fpl) Linear) ir_l |> Printer.save_picture "temp.ppm" 1;
  bfout_str

let pietevalstr () =
  Shexp_process.(eval(pipe (run "./npiet" ["temp.ppm"]) read_all))

let sink f x y =
  let _ = f x y in
  true

let all_but_print_t =
  let meta = Utils.MetaJson.get in
  QCheck.(Test.make
            ~count:100 ~name:"default_settings_work"
            (make bfprog_gen) (sink all_but_print meta))

let domains_fit_correctly_t =
  let meta = Utils.MetaJson.get in
  QCheck.(Test.make
            ~count:100 ~name:"Domains fit compactly."
            (make bfprog_gen) (domains_fit_correctly meta))

let full_t =
  let meta = Utils.MetaJson.get in
  QCheck.(Test.make
            ~count:100 ~name:"End_to_end"
            (make bfprog_gen) (fun s -> (=) (pietevalstr ()) @@ full meta s))

let _ = QCheck_runner.set_verbose true
let _ = QCheck_runner.run_tests_main [
    optimisation_is_stationary;
    all_but_print_t;
    domains_fit_correctly_t;
  ]
