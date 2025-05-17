type t = Spades | Hearts | Diamonds | Clubs

let color = function
  | Spades | Clubs -> Color.Black
  | Diamonds | Hearts -> Color.Red

let ansi_color t =
  t |> color |> Color.ansi_color

let to_string suit = match suit with
  | Spades -> "♠"
  | Hearts -> "♥"
  | Diamonds -> "♦"
  | Clubs -> "♣"

let to_colored_string suit =
  (suit |> ansi_color |> AnsiColor.to_string) ^ (suit |> to_string) ^ (AnsiColor.Reset |> AnsiColor.to_string)

let all = [Spades; Hearts; Diamonds; Clubs]
