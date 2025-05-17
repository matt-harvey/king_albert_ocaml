type t = Black | Red | Blue | Green | Reset

let to_string = function
  | Black -> "\027[30m"
  | Red -> "\027[31m"
  | Blue -> "\027[34m"
  | Green -> "\027[32m"
  | Reset -> "\027[0m"
