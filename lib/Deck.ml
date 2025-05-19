type t = Card.t array

let make_shuffled : t = Card.all |> Array.of_list |> Random.shuffle
