open StdLabels

let map_reduce
      (xs : 'a list)
      ~(m : 'a -> 'b)
      ~(r : 'c -> 'b -> 'c)
      ~(init : 'c) =
  List.fold_left xs ~init ~f:(fun acc x -> r acc (m x))

let map_reduce2
      (xs : 'a list)
      (ys : 'b list)
      ~(m : 'a -> 'b -> 'c)
      ~(r : 'd -> 'c -> 'd)
      ~(init : 'd) =
  List.fold_left2 xs ys ~init ~f:(fun acc x y -> r acc (m x y))

let sum_map (xs : 'a list) ~(m : 'a -> int) : int =
  map_reduce xs ~m ~r:(+) ~init:0

let sum_map2 (xs : 'a list) (ys : 'b list) ~(m : 'a -> 'b -> int) : int =
  map_reduce2 xs ys ~m ~r:(+) ~init:0
  
let count_where (xs : 'a list) ~(p : 'a -> bool) : int =
  sum_map xs ~m:(fun x -> if p x then 1 else 0)

let mem_freq (x : 'a) (xs : 'a list) ~(eq : 'a -> 'a -> bool) : int =
  count_where xs ~p:(eq x) 

let inner_map (xss : 'a list list) ~(f : 'a -> 'b) : 'b list list =
  List.map xss ~f:(fun xs -> List.map xs ~f)
