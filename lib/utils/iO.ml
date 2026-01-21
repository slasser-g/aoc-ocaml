open StdLabels
open Error
module IC = In_channel

let read_lines (fname : string) : string list =
  IC.with_open_text fname IC.input_lines

let split_ws (s : string) : string list =
  let r = Str.regexp "[ \t]+" in
  Str.split r s

let read_two_column_file (fname : string) : string list * string list =
  let lines = read_lines fname in
  List.fold_right lines ~init:([],[]) ~f:(fun line (xs,ys) ->
      match split_ws line with
      | [x;y] -> (x :: xs, y :: ys)
      | _ -> failwithf "Failed to read two fields from line %s" line)

let read_int_matrix (fname : string) : (int list) list =
  let lines = read_lines fname in
  let rows = List.map lines ~f:split_ws in
  ListUtils.inner_map rows ~f:int_of_string

let read_text (fname : string) : string =
  IC.with_open_text fname IC.input_all
