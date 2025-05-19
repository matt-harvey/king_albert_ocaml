type t = Rank.t * Suit.t

let rank (rank, _) = rank

let suit (_, suit) = suit

let color (_, suit) = suit |> Suit.color

let ansi_color (_, suit) = suit |> Suit.ansi_color

let to_styled_string (card : t) : string =
  (card |> ansi_color |> AnsiColor.to_string)
  ^ (card |> rank |> Rank.to_string)
  ^ (card |> suit |> Suit.to_unstyled_string)
  ^ (AnsiColor.Reset |> AnsiColor.to_string)

let all = Suit.all |> List.map (fun suit -> Rank.all |> List.map (fun rank -> (rank, suit))) |> List.flatten
