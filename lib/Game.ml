type t = {board: Board.t; mutable game_state: GameState.t}

let make = {board= Deck.make_shuffled |> Board.from_deck; game_state= GameState.Prepared}

let rec play out_channel in_channel game =
  game.game_state <- GameState.Playing ;
  while game.game_state <> GameState.Quit do
    Board.put out_channel game.board ;
    consume_move out_channel in_channel game
  done

and consume_move out_channel in_channel game =
  output_string out_channel (AnsiColor.to_string AnsiColor.Green) ;
  let board = game.board in
  let break_loop = ref false in
  while not !break_loop do
    output_string out_channel "Enter your move: " ;
    Out_channel.flush out_channel ;
    let input = In_channel.input_line in_channel in
    match input with
    | None -> ()
    | Some input -> (
        if input = "quit" || input = "exit" then (
          game.game_state <- GameState.Quit ;
          break_loop := true )
        else
          match Move.from_str input with
          | Option.Some {origin; destination} ->
              let origin_position = Board.position_at board origin in
              let destination_position = Board.position_at board destination in
              let origin_can_give = Position.can_give origin_position in
              if origin_can_give then
                let updated_origin_position, card_given = origin_position |> Position.give in
                match card_given with
                | Option.Some card -> (
                    let destination_can_receive = Position.can_receive destination_position card in
                    match destination_can_receive with
                    | false ->
                        output_string out_channel
                          "Invalid move. Please enter your move (two letters) or 'quit' to quit." ;
                        output_string out_channel "\n\n" ;
                        ()
                    | true ->
                        let updated_destionation_position = Position.receive destination_position card in
                        Board.update_position origin updated_origin_position board ;
                        Board.update_position destination updated_destionation_position board ;
                        break_loop := true )
                | Option.None -> ()
              else ()
          | Option.None ->
              output_string out_channel
                "Invalid move. Please enter your move (two letters) or 'quit' to quit." ;
              output_string out_channel "\n\n" ;
              () ;
              () )
  done
