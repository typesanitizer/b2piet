open Batteries
open OUnit2


let parser_tests =
  let parse = Parser.parse in
  (* let empty_bfipq = Q.create () in *)
  (* let empty_errq = Q.create () in *)
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

let optimisation_is_stationary =
  let f s =
    let (o1, _) = parse_opt s in
    let (o2, _) = Optimiser.optimise o1 in
    o1 = o2 in
  QCheck.(Test.make
            ~count:100
            ~name:"optimisation_is_stationary"
            (make bfprog_gen) f)

let all_but_print = parse_opt %> fst
                    %> Translator.translate %> fst
                    %> Painter.paint_linear_fast

let sink f k =
  let _ = f k in
  true

let all_but_print_t =
  QCheck.(Test.make
            ~count:100 ~name:"default_settings_work"
            (make bfprog_gen) @@ sink all_but_print)

let _ = QCheck_runner.set_verbose true
let _ = QCheck_runner.run_tests_main [
    optimisation_is_stationary;
    all_but_print_t;
  ]
