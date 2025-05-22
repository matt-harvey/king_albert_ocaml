type t = Game of Board.t

let make : t = Game (Deck.make_shuffled |> Board.from_deck)

let is_won game =
  let (Game board) = game in
  Board.is_won board

let rec play (out_channel : out_channel) (in_channel : in_channel) (game : t) : unit =
  let (Game board) = game in
  Board.put out_channel board ;
  while consume_move out_channel in_channel game = GameState.Playing do
    Board.put out_channel board
  done

and consume_move (out_channel : out_channel) (in_channel : in_channel) (game : t) : GameState.t =
  let put str = output_string out_channel str in
  output_string out_channel (AnsiColor.to_string AnsiColor.Green) ;
  let (Game board) = game in
  let game_state = ref GameState.Playing in
  let break_loop = ref false in
  while not !break_loop do
    put "Enter your move: " ;
    Out_channel.flush out_channel ;
    let input = In_channel.input_line in_channel in
    match input with
    | None -> ()
    | Some input -> (
        if input = "quit" || input = "exit" then (
          game_state := GameState.Quit ;
          break_loop := true )
        else
          match Move.from_str input with
          | Some {origin; destination} ->
              let origin_position = Board.position_at board origin in
              let destination_position = Board.position_at board destination in
              let origin_can_give = Position.can_give origin_position in
              if origin_can_give then
                let updated_origin_position, card_given = origin_position |> Position.give in
                match card_given with
                | None -> ()
                | Some card -> (
                    let destination_can_receive = Position.can_receive destination_position card in
                    match destination_can_receive with
                    | false -> put "Invalid move. Please enter your move (two letters) or 'quit' to quit.\n\n"
                    | true ->
                        let updated_destination_position = Position.receive destination_position card in
                        Board.update_position origin updated_origin_position board ;
                        Board.update_position destination updated_destination_position board ;
                        game_state :=
                          if Board.is_won board then (put "You won! Well done!\n\n" ; GameState.Won)
                          else GameState.Playing ;
                        break_loop := true )
              else ()
          | None ->
              put "Invalid move. Please enter your move (two letters) or 'quit' to quit.\n\n" ;
              () ;
              () )
  done ;
  !game_state
