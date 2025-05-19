module Game = King_albert.Game ;;

let game = Game.make in
Game.play Out_channel.stdout In_channel.stdin game
