type t = Position.t array

let from_deck (d : Deck.t) : t =
  [| Position.Foundation (0, Suit.Spades)
   ; Position.Foundation (0, Suit.Hearts)
   ; Position.Foundation (0, Suit.Diamonds)
   ; Position.Foundation (0, Suit.Clubs)
   ; Position.Column [d.(0)]
   ; Position.Column [d.(1); d.(9)]
   ; Position.Column [d.(2); d.(10); d.(17)]
   ; Position.Column [d.(3); d.(11); d.(18); d.(24)]
   ; Position.Column [d.(4); d.(12); d.(19); d.(25); d.(30)]
   ; Position.Column [d.(5); d.(13); d.(20); d.(26); d.(31); d.(35)]
   ; Position.Column [d.(6); d.(14); d.(21); d.(27); d.(32); d.(36); d.(39)]
   ; Position.Column [d.(7); d.(15); d.(22); d.(28); d.(33); d.(37); d.(40); d.(42)]
   ; Position.Column [d.(8); d.(16); d.(23); d.(29); d.(34); d.(38); d.(41); d.(43); d.(44)]
   ; Position.Reserve (Option.Some d.(45))
   ; Position.Reserve (Option.Some d.(46))
   ; Position.Reserve (Option.Some d.(47))
   ; Position.Reserve (Option.Some d.(48))
   ; Position.Reserve (Option.Some d.(49))
   ; Position.Reserve (Option.Some d.(50))
   ; Position.Reserve (Option.Some d.(51)) |]

let position_at (board : t) (index : int) = board.(index)

let max_column_length (board : t) : int =
  Array.fold_left
    (fun acc pos ->
      match pos with
      | Position.Column cards ->
          let column_length = List.length cards in
          if column_length > acc then column_length else acc
      | _ -> acc )
    0 board

let update_position (index : int) (new_position : Position.t) (board : t) = board.(index) <- new_position

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
  for _ = 1 to 5 do
    output_string out blank
  done ;
  for i = 0 to 3 do
    let pos : Position.t = board.(i) in
    let col_str : string = Position.to_string_at pos 0 in
    output_string out " " ; output_string out col_str
  done ;
  output_string out "\n\n" ;
  (* Print references to columns *)
  output_string out "\n" ;
  output_string out (AnsiColor.to_string AnsiColor.Blue) ;
  output_string out " [e] [f] [g] [h] [i] [j] [k] [l] [m]" ;
  output_string out (AnsiColor.to_string AnsiColor.Reset) ;
  output_string out "\n\n" ;
  (* Print columns *)
  let max = max_column_length board in
  for row_index = 1 to max do
    for column_index = 4 to 12 do
      let pos : Position.t = board.(column_index) in
      output_string out " " ;
      output_string out (Position.to_string_at pos (row_index - 1))
    done ;
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
  for reserve_index = 13 to 19 do
    let pos : Position.t = board.(reserve_index) in
    output_string out " " ;
    output_string out (Position.to_string_at pos 0)
  done ;
  output_string out "\n\n" ;
  Out_channel.flush out
