module Card = King_albert.Card;;
module Deck = King_albert.Deck;;

Deck.make_shuffled |> Array.iter (fun card -> card |> Card.to_styled_string |> print_endline)