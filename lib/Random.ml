let shuffle t =
  (* For some reason we have to seed the randomizer here, not outside this function call *)
  Unix.time () |> int_of_float |> Containers.Random.init ;
  Containers.Array.shuffle t ;
  t
