type t = {board: Board.t; state: GameState.t}

let make : t = {board= Deck.make_shuffled |> Board.from_deck; state= GameState.Playing}
let is_playing = function {board= _; state= GameState.Playing} -> true | _ -> false

let rec play (out_channel : out_channel) (in_channel : in_channel) (game : t) : unit =
  game |> game_snapshots out_channel in_channel |> Seq.iter (fun _ -> ())

and put (out_channel : out_channel) ({board; state} : t) : unit =
  match state with
  | GameState.Playing -> Board.put out_channel board
  | GameState.Quit -> ()
  | GameState.Won ->
      output_string out_channel "Congratulation! You won!\n\n" ;
      Out_channel.flush out_channel

and game_snapshots (out_channel : out_channel) (in_channel : in_channel) =
  Seq.unfold (fun g ->
      match g with
      | {board= _; state= GameState.Playing} -> Some (g, consume_move out_channel in_channel g)
      | _ -> None )

and consume_move (out_channel : out_channel) (in_channel : in_channel) (game : t) : t =
  put out_channel game ;
  let put_message str = output_string out_channel str in
  output_string out_channel (AnsiColor.to_string AnsiColor.Green) ;
  let {board; state} = game in
  let game_state = ref state in
  let break_loop = ref false in
  let new_board = ref board in
  while not !break_loop do
    put_message "Enter your move: " ;
    Out_channel.flush out_channel ;
    match In_channel.input_line in_channel with
    | None -> ()
    | Some input -> (
      match Move.from_str input with
      | None -> put_message "Invalid move. Please enter your move (two letters) or 'quit' to quit.\n\n"
      | Some Move.Exit ->
          game_state := GameState.Quit ;
          break_loop := true
      | Some (Move.CardMove {origin; destination}) ->
          let origin_position = board |> Board.position_at origin in
          let destination_position = board |> Board.position_at destination in
          if Position.can_give origin_position then
            let updated_origin_position, card_given = origin_position |> Position.give in
            match card_given with
            | None -> ()
            | Some card ->
                if Position.can_receive destination_position card then (
                  let updated_destination_position = Position.receive destination_position card in
                  new_board := board
                  |> Board.update_position origin updated_origin_position
                  |> Board.update_position destination updated_destination_position;
                  game_state := if Board.is_won !new_board then GameState.Won else GameState.Playing ;
                  break_loop := true )
                else put_message "Invalid move. Please enter your move (two letters) or 'quit' to quit.\n\n"
          else () )
  done ;
  {board= !new_board; state= !game_state}
