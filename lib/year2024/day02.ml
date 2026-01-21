open Utils

(**************)
(*   PART 1   *)
(**************)

let rec forall_intervals (xs : int list) ~(p: int -> int -> bool) : bool = 
  match xs with
    | [] | [_] -> true
    | x :: y :: zs -> p x y && forall_intervals (y :: zs) ~p

let all_increasing (xs : int list) : bool =
  forall_intervals xs ~p:(<=)

let all_decreasing (xs : int list) : bool =
  forall_intervals xs ~p:(>=)

let small_diffs (xs : int list) : bool =
  let diff x y = Int.abs @@ x - y in
  let small_diff x y =
    let d = diff x y in
    1 <= d && d <= 3 in
  forall_intervals xs ~p:small_diff

let safe_report (xs : int list) : bool =
  (all_increasing xs || all_decreasing xs) && small_diffs xs

let part1 infile =
  let reports = IO.read_int_matrix infile in
  let soln = ListUtils.count_where reports ~p:safe_report in
  string_of_int soln

(**************)
(*   PART 2   *)
(**************)

let for_almost_all_intervals (xs : int list) ~(p : int -> int -> bool) : bool =
  let rec iter rev_prev = function
    | [] | [_] -> true
    | x :: y :: zs ->
       if p x y then iter (x :: rev_prev) (y :: zs) else
         let prev = List.rev rev_prev in
         forall_intervals (prev @ x :: zs) ~p ||
         forall_intervals (prev @ y :: zs) ~p
  in iter [] xs

let mostly_safe_report (xs : int list) : bool =
  let small_increase x y =
    let step = y - x in
    1 <= step && step <= 3
  in
  let small_decrease x y =
    let step = y - x in
    -3 <= step && step <= -1
  in
  for_almost_all_intervals xs ~p:small_increase ||
  for_almost_all_intervals xs ~p:small_decrease

let part2 infile =
  let reports = IO.read_int_matrix infile in
  let soln = ListUtils.count_where reports ~p:mostly_safe_report in
  string_of_int soln
