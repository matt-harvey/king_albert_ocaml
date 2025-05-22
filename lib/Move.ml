type t = {origin: int; destination: int}

let char_to_position_index (ch : char) : int option =
  if ch < 'a' || ch > 't' then None else Some(Char.code(ch) - Char.code('a'))

let from_str (s : string) : t option =
  if String.length s <> 2 then
    None
  else
    match char_to_position_index s.[0], char_to_position_index s.[1] with
    | Some origin, Some destination -> Some {origin; destination}
    | _ -> None