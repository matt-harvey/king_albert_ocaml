type t = int

let ace = 1
let king = 13

let min = ace
let max = king

let all : t list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13]

let to_string t = match t with
  | 1 -> " A"
  | 10 -> "10"
  | 11 -> " J"
  | 12 -> " Q"
  | 13 -> " K"
  | _ -> Printf.sprintf " %d" t
