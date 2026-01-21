let extract_int (g : Re.Group.t) (i : int) =
  Re.Group.get g i |> int_of_string
