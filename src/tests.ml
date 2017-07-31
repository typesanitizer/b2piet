open Batteries
open OUnit2


let parser_tests =
  let parse = Parser.parse in
  "Test suite for Parser" >::: [
    "empty" >:: (fun _ -> assert_equal ([], []) ("" |> parse) );
    "instr" >:: (fun _ ->
        assert_equal [] ("><+-[].," |> parse |> snd));
    "unicode" >:: (fun _ ->
        assert_equal ([], []) ("αω·ΑΩ\n\t" |> parse));
  ]

(* let _ = run_test_tt_main parser_tests *)

open Optimiser
let optimiser_tests =
  let parse = Parser.parse in
  let parse_l = fun s -> s |> parse |> fst in
  "Test suite for Optimiser" >::: [
    "empty" >:: (fun _ -> assert_equal ([],[]) ("" |> parse_l |> optimise));
  ]

(* let _ = run_test_tt_main optimiser_tests *)

let bfprog_gen = QCheck.Gen.(
    let linear_codegen =
      string_size (int_range 1 4)
        ~gen:(oneofl ['+'; '-'; '<'; '>'; '.'; ',']) in
    sized @@ fix (fun self -> function
        | 0 -> linear_codegen
        | n -> frequency
                 [4, map2 (^) linear_codegen @@ self (n/2);
                  1, map (fun s -> (List.fold_left (^) "[" s ) ^ "]")
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
            (make bfprog_gen) @@ sink all_but_print meta)

let full_t =
  let meta = Utils.MetaJson.get in
  QCheck.(Test.make
            ~count:100 ~name:"End_to_end"
            (make bfprog_gen) @@ fun s -> (=) (pietevalstr ()) @@ full meta s
         )

let _ = QCheck_runner.set_verbose true
let _ = QCheck_runner.run_tests_main [
    optimisation_is_stationary;
    all_but_print_t;
  ]
