module Suit = King_albert.Suit;;

Suit.all |> List.iter (fun suit -> suit |> Suit.to_colored_string |> print_endline)
