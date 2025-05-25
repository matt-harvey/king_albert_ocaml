type t = CardMove of {origin: char; destination: char} | Exit

let char_to_position_index (ch : char) : char option =
  if ch < 'a' || ch > 't' then None else Some ch

let from_str (s : string) : t option =
  if s = "quit" || s = "exit" then Some Exit
  else if String.length s <> 2 then None
  else
    match (char_to_position_index s.[0], char_to_position_index s.[1]) with
    | Some origin, Some destination -> Some (CardMove {origin; destination})
    | _ -> None
