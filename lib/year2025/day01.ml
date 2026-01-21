open StdLabels
open Utils
open Error

(**************)
(*   PART 1   *)
(**************)

type rotation =
  | L of int
  | R of int

let int_of_rotation : rotation -> int = function
  | L i -> -i
  | R i ->  i

let show_rotation : rotation -> string = function
  | L i -> "L" ^ string_of_int i
  | R i -> "R" ^ string_of_int i

let rotation_regex : Re.re =
  let open Re in
  let dir_pat = alt [char 'L'; char 'R'] in
  let mag_pat = rep1 digit in
  let pattern = seq [group dir_pat; group mag_pat] in
  Re.compile pattern

let extract_rotation (line : string) : rotation =
  let group = Re.exec rotation_regex line in
  let dir = Re.Group.get group 1 in
  let mag = Re.Group.get group 2 |> int_of_string in
  match dir with
  | "L" -> L mag
  | "R" -> R mag
  | _ -> failwithf "Unrecognized direction: %s" dir

let modulo (n : int) (m : int) : int =
  (n mod m + m) mod m

let count_zeros (rs : rotation list) ~(init_dial : int) ~(m : int) : int =
  let rec iter (rs : rotation list) ~(dial : int) ~(zeros : int) : int =
    match rs with
    | [] -> zeros
    | r :: rs' ->
       let delta = int_of_rotation r in
       let dial' = modulo (dial + delta) m in
       let zeros' = if dial' = 0 then zeros + 1 else zeros in
       iter rs' ~dial:dial' ~zeros:zeros'
  in
  iter rs ~dial:init_dial ~zeros:(if init_dial = 0 then 1 else 0)
  
let part1 infile =
  let lines = IO.read_lines infile in
  let rotations = List.map lines ~f:extract_rotation in
  let zeros = count_zeros rotations ~init_dial:50 ~m:100 in
  string_of_int zeros

(**************)
(*   PART 2   *)
(**************)

let count_zero_clicks (rs : rotation list) ~(init_dial : int) ~(m : int) : int =
  let rec iter (rs : rotation list) ~(dial : int) ~(zero_clicks : int) : int =
    match rs with
    | [] -> zero_clicks
    | L delta :: rs' ->
       let sum = modulo (m - dial) m + delta in
       let dial' = modulo (m - sum) m in
       let zero_clicks' = zero_clicks + (sum / m) in
       iter rs' ~dial:dial' ~zero_clicks:zero_clicks'
    | R delta :: rs' ->
       let sum = dial + delta in
       let dial' = modulo sum m in
       let zero_clicks' = zero_clicks + (sum / m) in
       iter rs' ~dial:dial' ~zero_clicks:zero_clicks'
  in 
  iter rs ~dial:init_dial ~zero_clicks:0

let part2 infile =
  let lines = IO.read_lines infile in
  let rotations = List.map lines ~f:extract_rotation in
  let zero_clicks = count_zero_clicks rotations ~init_dial:50 ~m:100 in
  string_of_int zero_clicks

