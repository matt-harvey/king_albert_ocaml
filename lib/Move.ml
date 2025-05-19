type t = {origin: int; destination: int}

let char_to_pos_idx ch =
  match ch with
  | 'a' -> Option.Some 0
  | 'b' -> Option.Some 1
  | 'c' -> Option.Some 2
  | 'd' -> Option.Some 3
  | 'e' -> Option.Some 4
  | 'f' -> Option.Some 5
  | 'g' -> Option.Some 6
  | 'h' -> Option.Some 7
  | 'i' -> Option.Some 8
  | 'j' -> Option.Some 9
  | 'k' -> Option.Some 10
  | 'l' -> Option.Some 11
  | 'm' -> Option.Some 12
  | 'n' -> Option.Some 13
  | 'o' -> Option.Some 14
  | 'p' -> Option.Some 15
  | 'q' -> Option.Some 16
  | 'r' -> Option.Some 17
  | 's' -> Option.Some 18
  | 't' -> Option.Some 19
  | _ -> Option.None

let from_str (s : string) : t option =
  match String.length s with
  | 2 -> (
    match (char_to_pos_idx s.[0], char_to_pos_idx s.[1]) with
    | Option.Some origin, Option.Some destination -> Option.Some {origin; destination}
    | _ -> Option.None )
  | _ -> Option.None
