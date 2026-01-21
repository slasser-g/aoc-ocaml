open StdLabels
open Utils

(**************)
(*   PART 1   *)
(**************)

type mul = int * int

let eval_mul (x, y) : int = x * y

let mul_pat : Re.t =
  let open Re in
  let num = group @@ repn digit 1 (Some 3) in
  seq [str "mul(" ; num ; char ',' ; num ; char ')']

let mul_regex : Re.re =
  Re.compile mul_pat

let extract_int (g : Re.Group.t) (i : int) =
  Re.Group.get g i |> int_of_string

let extract_mul_indices (g : Re.Group.t) (i : int) (j : int) : mul =
  (extract_int g i, extract_int g j)

let extract_mul (g : Re.Group.t) : mul =
  extract_mul_indices g 1 2

let part1 infile =
  let text = IO.read_text infile in
  let groups = Re.all mul_regex text in
  let muls = List.map groups ~f:extract_mul in
  let soln = ListUtils.sum_map muls ~m:eval_mul in
  string_of_int soln

(**************)
(*   PART 2   *)
(**************)

type insn =
  | Mul of mul
  | Do
  | Don't

let insn_regex : Re.re =
  let open Re in
  let do_pat = str "do()" in
  let don't_pat = str "don't()" in
  let pat = alt @@ List.map ~f:group [mul_pat ; do_pat ; don't_pat] in
  Re.compile pat

let extract_insn (g : Re.Group.t) : insn =
  let test i = Re.Group.test g i in
  if test 1 then
    Mul (extract_mul_indices g 2 3)
  else if test 4 then
    Do
  else if test 5 then
    Don't
  else
    failwith "couldn't extract instruction from group"

let exec (insns : insn list) : int =
  let rec iter (insns : insn list) (enabled : bool) (acc : int) : int =
    match insns with
    | [] -> acc
    | Mul m :: tl ->
       let acc' = if enabled then acc + eval_mul m else acc in
       iter tl enabled acc'
    | Do :: tl -> iter tl true acc
    | Don't :: tl -> iter tl false acc
  in
  iter insns true 0

let part2 infile =
  let text = IO.read_text infile in
  let groups = Re.all insn_regex text in
  let insns = List.map groups ~f:extract_insn in
  let soln = exec insns in
  string_of_int soln
