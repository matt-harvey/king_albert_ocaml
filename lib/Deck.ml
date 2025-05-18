let make_shuffled =
  Card.all |> Array.of_list |> Random.shuffle
