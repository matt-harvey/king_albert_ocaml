module CharMap = Map.Make (CharMapMod)

type t = Position.t CharMap.t

let from_deck (d : Deck.t) : t = CharMap.empty
  |> CharMap.add 'a' (Position.Foundation (0, Suit.Spades))
  |> CharMap.add 'b' (Position.Foundation (0, Suit.Hearts))
  |> CharMap.add 'c' (Position.Foundation (0, Suit.Diamonds))
  |> CharMap.add 'd' (Position.Foundation (0, Suit.Clubs))
  |> CharMap.add 'e' (Position.Column [d.(0)])
  |> CharMap.add 'f' (Position.Column [d.(1); d.(9)])
  |> CharMap.add 'g' (Position.Column [d.(2); d.(10); d.(17)])
  |> CharMap.add 'h' (Position.Column [d.(3); d.(11); d.(18); d.(24)])
  |> CharMap.add 'i' (Position.Column [d.(4); d.(12); d.(19); d.(25); d.(30)])
  |> CharMap.add 'j' (Position.Column [d.(5); d.(13); d.(20); d.(26); d.(31); d.(35)])
  |> CharMap.add 'k' (Position.Column [d.(6); d.(14); d.(21); d.(27); d.(32); d.(36); d.(39)])
  |> CharMap.add 'l' (Position.Column [d.(7); d.(15); d.(22); d.(28); d.(33); d.(37); d.(40); d.(42)])
  |> CharMap.add 'm' (Position.Column [d.(8); d.(16); d.(23); d.(29); d.(34); d.(38); d.(41); d.(43); d.(44)])
  |> CharMap.add 'n' (Position.Reserve (Option.Some d.(45)))
  |> CharMap.add 'o' (Position.Reserve (Option.Some d.(46)))
  |> CharMap.add 'p' (Position.Reserve (Option.Some d.(47)))
  |> CharMap.add 'r' (Position.Reserve (Option.Some d.(48)))
  |> CharMap.add 's' (Position.Reserve (Option.Some d.(49)))
  |> CharMap.add 't' (Position.Reserve (Option.Some d.(50)))
  |> CharMap.add 'u' (Position.Reserve (Option.Some d.(51)))

let position_at (key : char) (board : t): Position.t = CharMap.find key board

let positions (board : t) : Position.t list =
  board
  |> CharMap.bindings
  |> List.split
  |> snd

let foundations (board : t) : Position.t list =
  board
  |> positions
  |> List.filter (fun v -> match v with Position.Foundation(_, _) -> true | _ -> false)

let columns (board : t) : Position.t list =
  board
  |> positions
  |> List.filter (fun v -> match v with Position.Column(_) -> true | _ -> false)

let reserves (board : t) : Position.t list =
  board
  |> positions
  |> List.filter (fun v -> match v with Position.Reserve(_) -> true | _ -> false)

let is_won board = board |> foundations |> List.for_all Position.is_complete_foundation

let max_column_length (board : t) : int =
  board
  |> positions
  |> List.fold_left
      (fun acc pos ->
        match pos with
        | Position.Column cards ->
            let column_length = List.length cards in
            if column_length > acc then column_length else acc
        | _ -> acc )
      0

let update_position (key : char) (new_position : Position.t) (board : t) =
  board |> CharMap.add key new_position

let clear_screen = "\027[H\027[J"

let put (out : Out_channel.t) (board : t) =
  output_string out clear_screen ;
  output_string out clear_screen ;
  Out_channel.flush out ;
  let blank : string = "    " in
  (* Print references to foundations *)
  output_string out "\n" ;
  for _ = 1 to 5 do
    output_string out blank
  done ;
  output_string out (AnsiColor.to_string AnsiColor.Blue) ;
  output_string out " [a] [b] [c] [d]" ;
  output_string out (AnsiColor.to_string AnsiColor.Reset) ;
  output_string out "\n\n" ;
  (* Print foundations *)
  for _ = 1 to 5 do output_string out blank done ;
  foundations board
  |> List.iter (fun f ->
    let col_str : string = Position.to_string_at f 0 in
    output_string out " " ; output_string out col_str
  );
  output_string out "\n\n" ;
  (* Print references to columns *)
  output_string out "\n" ;
  output_string out (AnsiColor.to_string AnsiColor.Blue) ;
  output_string out " [e] [f] [g] [h] [i] [j] [k] [l] [m]" ;
  output_string out (AnsiColor.to_string AnsiColor.Reset) ;
  output_string out "\n\n" ;
  (* Print columns *)
  let max = max_column_length board in
  let column_positions = columns board in
  for row_index = 1 to max do
    column_positions |> List.iter (fun col ->
      output_string out " " ;
      output_string out (Position.to_string_at col (row_index - 1))
    );
    output_string out "\n"
  done ;
  output_string out "\n\n" ;
  (* Print references to reserves *)
  output_string out "\n" ;
  output_string out (AnsiColor.to_string AnsiColor.Blue) ;
  output_string out " [n] [o] [p] [q] [r] [s] [t]" ;
  output_string out (AnsiColor.to_string AnsiColor.Reset) ;
  output_string out "\n\n" ;
  (* Print reserves *)
  reserves board
  |> List.iter (fun r ->
    output_string out " " ;
    output_string out (Position.to_string_at r 0)
  );
  output_string out "\n\n" ;
  Out_channel.flush out
