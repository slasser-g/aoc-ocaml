let map2 (pr : 'a * 'a) ~(f : 'a -> 'b) : 'b * 'b =
  Pair.map f f pr
  
