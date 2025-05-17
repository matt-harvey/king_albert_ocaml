module Card = King_albert.Card;;

Card.all |> List.iter (fun card -> card |> Card.to_styled_string |> print_endline)
