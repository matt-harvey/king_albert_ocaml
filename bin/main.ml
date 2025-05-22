module Game = King_albert.Game ;;

Game.make |> Game.play Out_channel.stdout In_channel.stdin
