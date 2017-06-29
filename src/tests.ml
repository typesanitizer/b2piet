open Batteries
open OUnit2
open Utils
open Parser

module Q = Queue

let parser_tests =
  (* let empty_bfipq = Q.create () in *)
  (* let empty_errq = Q.create () in *)
  "Test suite for Parser" >::: [
    "empty" >:: (fun _ -> assert_equal ([], []) ("" |> parse) );
    "instr" >:: (fun _ ->
        assert_equal [] ("><+-[].," |> parse |> snd));
    "unicode" >:: (fun _ ->
        assert_equal ([], []) ("αω·ΑΩ\n\t" |> parse));
  ]

let _ = run_test_tt_main parser_tests

open Optimiser
let optimiser_tests =
  let parse_l = fun s -> s |> parse |> fst in
  "Test suite for Optimiser" >::: [
    "empty" >:: (fun _ -> assert_equal ([],[]) ("" |> parse_l |> optimise));
  ]

let _ = run_test_tt_main optimiser_tests
