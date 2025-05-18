module Card = King_albert.Card;;
module Deck = King_albert.Deck;;
module Board = King_albert.Board;;

Deck.make_shuffled |> Board.from_deck |> Board.put stdout
