let usage () =
  Printf.eprintf "Usage: %s <day> <part>\n" Sys.argv.(0);
  Printf.eprintf "  day:  1-25\n";
  Printf.eprintf "  part: 1 or 2\n";
  exit 1

let read_args () : int * int * int =
  let int_arg i =
    try
      int_of_string Sys.argv.(i)
    with _ -> usage ()
  in
  let year, day, part = int_arg 1, int_arg 2, int_arg 3 in
  (year, day, part)

let get_input_path (year : int) (day : int) : string =
  Printf.sprintf "input/year%d/day%02d.txt" year day

let print_solution (year : int) (day : int) (part : int) (soln : string) : unit =
  let template = format_of_string
{|
****
Year    : %d
Day     : %d
Part    : %d
Solution: %s
****

|}
  in Printf.printf template year day part soln
  
let main () =
  let year, day, part = read_args () in
  let input_path = get_input_path year day in
  let solve = match year, day, part with
    | 2024, 1, 1 -> Year2024.Day01.part1 
    | 2024, 1, 2 -> Year2024.Day01.part2
    | 2024, 2, 1 -> Year2024.Day02.part1
    | 2024, 2, 2 -> Year2024.Day02.part2
    | 2024, 3, 1 -> Year2024.Day03.part1
    | 2024, 3, 2 -> Year2024.Day03.part2
    | 2025, 1, 1 -> Year2025.Day01.part1
    | 2025, 1, 2 -> Year2025.Day01.part2
    (* Add more days here *)
    | y, d, p ->
        Printf.eprintf "Error: Year %d, Day %d, Part %d not implemented\n" y d p;
        exit 1
  in
  let soln = solve input_path in
  print_solution year day part soln 

let () = main ()
