open Batteries

module Range = struct
  type t = int * int
  [@@deriving show {with_path = false}]
  type s = int

  let t_compare (t1_l, t1_u) (t2_l, t2_u) =
    if t1_u < t2_l then Utils.LT
    else if t2_u < t1_l then Utils.GT
    else Utils.EQ

  let s_inside_t i (t_l, t_u) =
    if i < t_l then Utils.LT
    else if i > t_u then Utils.GT
    else Utils.EQ
end

module ST = Utils.SplayTree(Range)

let range_list_gen = QCheck.Gen.(
    let rec f n acc = function
      | x :: xs -> f (n + x + 1) ((n, n + x) :: acc) xs
      | [] -> acc in
    sized @@ fix (fun self -> function
        | 0 -> return []
        | n ->
          list (int_range 1 n)
          |> map (f 0 [] %> List.rev)
      )
  )

let st_of_list l =
  let n = (if l = [] then None
           else List.last l |> fun (_, n) -> Some n) in
  List.shuffle l
  |> List.fold_left (flip ST.insert) ST.empty
  |> fun st -> (st, n)

let st_insert (st, n) =
  let k = QCheck.Gen.(generate1 nat) in
  match n, fst (ST.find_s k st) with
  | None, None -> true
  | Some n, None when k > n -> true
  | Some n, Some (k', k'') when k' <= k && k <= k'' -> true
  | _ -> false

let st_insert_t = QCheck.(
    let arb_st = make @@ Gen.map st_of_list range_list_gen in
    Test.make ~count:100 ~name:"SplayTree insertion."
      arb_st st_insert
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

let bfprog_gen ~input = QCheck.Gen.(
    let linear_codegen =
      let instr = (if input then [','] else []) @ ['+'; '-'; '<'; '>'; '.'] in
      string_size (int_range 1 4) ~gen:(oneofl instr) in

    sized @@ fix (fun self -> function
        | 0 -> linear_codegen
        | n -> frequency
                 [
                   4, map2 (^) linear_codegen @@ self (n / 2);
                   1, map2 (^) (map (fun s -> "[" ^ s ^ "]") @@ self (n / 2))
                     @@ self n
                 ]
      )
  )

let bfprog_shrink s yield =
  let _ = print_endline ("shrinking: " ^ s) in
  let len = String.length s in
  if len = 1 then ()
  else
    let imax = len - 1 in
    for i = 0 to imax - 1 do
      let s' =
        if s.[i] = '[' && i < imax && s.[i + 1] = ']' then
          Some (Bytes.init (imax - 1)
                  (fun j -> if j < i then s.[j] else s.[j + 2]))
        else if s.[i] = '[' || s.[i] = ']' then
          None
        else
          Some (Bytes.init (String.length s - 1)
                  (fun j -> if j < i then s.[j] else s.[j + 1]))
      in
      match s' with
      | Some s' -> yield (Bytes.unsafe_to_string s')
      | None -> ()
    done

let bfprog_make ~input =
  QCheck.make ~shrink:bfprog_shrink ~print:identity (bfprog_gen ~input)

let optimisation_is_stationary =
  let f s =
    let (o1, _) = parse_opt s in
    let (o2, _) = Optimiser.optimise o1 in
    o1 = o2 in
  QCheck.(Test.make
            ~count:100
            ~name:"Optimisation is stationary"
            (bfprog_make ~input:true) f)

let temp_fname = "temp.png"

let read_from_process stream =
  let buffer = Buffer.create 4096 in
  try%lwt
    let%lwt () =
      Lwt_stream.iter (Buffer.add_string buffer
                       %> fun () -> Buffer.add_char buffer '\n') stream in
    Lwt.return (Ok (Buffer.contents buffer))
  with Lwt_io.Channel_closed _ ->
    Lwt.return (Bad (Buffer.contents buffer))

let pietevalstr () =
  let process_stdin, push_line = Lwt_stream.create () in
  let process_stdout =
    Lwt_process.pmap_lines
      ~timeout:3.0 ("./fpiet", [|"./fpiet"; temp_fname|]) process_stdin in
  read_from_process process_stdout

let both = ref 0
let interpret_timeout = ref 0
let interpret_err = ref 0
let fpiet_timeout = ref 0

let is_prefix_of pre_str big_str =
  String.(equal pre_str (slice ~last:(String.length pre_str) big_str))

let print_char_nums =
  String.to_list %> List.map int_of_char %>
  List.print (fun _ -> print_int) stdout

let end_to_end meta str =
  (* >>> prevents too many interpreter errors and . checks the final value *)
  let str = ">>>" ^ str ^ "." in
  let bfinstr = (str |> Parser.parse |> fst) in

  let comp =
    Lwt.map (fun a -> Some a) (Translator.interpret_woutput bfinstr) in
  let timeout =
    let%lwt () = Lwt_unix.sleep 3.0 in
    Lwt.return None in

  match Lwt_main.run (Lwt.pick [comp; timeout]) with
  | Some (stack_size, bfout_str, None) ->
    let (ir_l, _) = Translator.translate ~modulo:true ~stack_size bfinstr in
    let (_, fpl) = Utils.MetaJson.get_fast_push_table ~stack_size meta in
    Painter.(paint (Fast fpl) Linear) ir_l |> Printer.save_picture temp_fname 1;
    begin
      match Lwt_main.run (pietevalstr ()) with
      | Bad s -> (
          incr fpiet_timeout;
          is_prefix_of s bfout_str
        )
      | Ok s ->
        (* remove last \n *)
        let s = String.slice ~last:(String.length s - 1) s in
        incr both;
        String.equal bfout_str s
    end
  | Some (_, _, Some _) -> incr interpret_err; true
  | None -> incr interpret_timeout; true

let no_crash f x y =
  try
    let _ = f x y in
    true
  with
    _ -> false

let all_but_print_t =
  let meta = Utils.MetaJson.get in
  QCheck.Test.make
    ~count:100 ~name:"No crashes (modulo print)."
    (bfprog_make ~input:true) (no_crash all_but_print meta)

let domains_fit_correctly_t =
  let meta = Utils.MetaJson.get in
  QCheck.Test.make
    ~count:100 ~name:"Domains fit correctly."
    (bfprog_make ~input:true) (domains_fit_correctly meta)

let end_to_end_t =
  let meta = Utils.MetaJson.get in
  QCheck.Test.make
    ~count:100 ~name:"End to end."
    (bfprog_make ~input:false) (end_to_end meta)

let _ = QCheck_runner.set_verbose true
let _ = QCheck_runner.run_tests [
    optimisation_is_stationary;
    all_but_print_t;
    domains_fit_correctly_t;
    st_insert_t;
    end_to_end_t;
  ]

let () = print_endline @@ Printf.sprintf
    "both complete %d\n\
     fpiet timeout %d\n\
     intrp timeout %d\n\
     interpret err %d\n"
    !both !fpiet_timeout !interpret_timeout !interpret_err
