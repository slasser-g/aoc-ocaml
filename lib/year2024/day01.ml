open StdLabels
open Utils
open MapUtils

(**************)
(*   PART 1   *)
(**************)

let sum_diffs (xs : int list) (ys : int list) : int =
  let diff x y = Int.abs @@ x - y in
  ListUtils.sum_map2 xs ys ~m:diff

let sort_as_ints (xs : string list) : int list =
  xs |> List.map ~f:int_of_string |> List.sort ~cmp:Int.compare

let part1 infile =
  let xs  , ys  = IO.read_two_column_file infile in
  let xs' , ys' = PairUtils.map2 (xs, ys) ~f:sort_as_ints in
  let sum = sum_diffs xs' ys' in
  string_of_int sum

(**************)
(*   PART 2   *)
(**************)

let ints_of_strings (xs : string list) : int list =
  List.map xs ~f:int_of_string

type freq_map = int IntMap.t

let make_freq_map (xs : int list) (ys : int list) : freq_map =
  List.fold_left xs ~init:IntMap.empty ~f:(fun m x ->
      if IntMap.mem x m then m
      else let freq = ListUtils.mem_freq x ys ~eq:Int.equal in
           IntMap.add x freq m)

let sim_score (xs : int list) (fm : freq_map) : int =
  ListUtils.sum_map xs ~m:(fun x -> x * IntMap.find x fm)

let part2 infile =
  let xs  , ys  = IO.read_two_column_file infile in
  let xs' , ys' = PairUtils.map2 (xs, ys) ~f:ints_of_strings in
  let fm = make_freq_map xs' ys' in
  let soln = sim_score xs' fm in
  string_of_int soln
