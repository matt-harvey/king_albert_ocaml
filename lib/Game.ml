type t = {board: Board.t; state: GameState.t}

let make : t = {board= Deck.make_shuffled |> Board.from_deck; state= GameState.Playing}

let rec play (out_channel : out_channel) (in_channel : in_channel) (game : t) : unit =
  let {board; _} = game in
  Board.put out_channel board ;
  while {board ; state=GameState.Playing} = consume_move out_channel in_channel game do
    Board.put out_channel board
  done

and consume_move (out_channel : out_channel) (in_channel : in_channel) (game : t) : t =
  let put str = output_string out_channel str in
  output_string out_channel (AnsiColor.to_string AnsiColor.Green) ;
  let {board; state} = game in
  let game_state = ref state in
  let break_loop = ref false in
  while not !break_loop do
    put "Enter your move: " ;
    Out_channel.flush out_channel ;
    match In_channel.input_line in_channel with
    | None -> ()
    | Some "quit" | Some "exit" ->
        game_state := GameState.Quit ;
        break_loop := true
    | Some input -> (
        match Move.from_str input with
        | None -> put "Invalid move. Please enter your move (two letters) or 'quit' to quit.\n\n"
        | Some {origin; destination} ->
            let origin_position = Board.position_at board origin in
            let destination_position = Board.position_at board destination in
            if Position.can_give origin_position then
              let updated_origin_position, card_given = origin_position |> Position.give in
              match card_given with
              | None -> ()
              | Some card ->
                  if Position.can_receive destination_position card then
                      let updated_destination_position = Position.receive destination_position card in
                      Board.update_position origin updated_origin_position board ;
                      Board.update_position destination updated_destination_position board ;
                      game_state :=
                        if Board.is_won board then (put "You won! Well done!\n\n" ; GameState.Won)
                        else GameState.Playing ;
                      break_loop := true
                  else
                    put "Invalid move. Please enter your move (two letters) or 'quit' to quit.\n\n";
            else () )
  done ;
  {board; state = !game_state}
