open Game
open Games
open GameInProgress
open Types

module RDB (T : sig type t end) =
  struct
    let objects = ref []

    let put id t = objects := (id,t) :: !objects
    let delete id = objects := List.filter (fun (x,_) -> x <> id) !objects

    let get id = List.assoc id !objects
    let get_channel id = fst (get id)
    let get_update_function id = snd (get id)
  end


module Games = MemoryGames
module TTTGameF = Game(Ttt.Board)
module TTTGameInProgress = GameInProgress(TTTGameF)(Ttt.XOPiece)
module TTTFBGame = FrontAndBackendReactGame.FrontAndBackendReactGame(GameInProgress)(TTTGameF)(Ttt.XOPiece)(RDB)
module TTTGames = Games(FrontAndBackendReactGame.FrontAndBackendReactGame)(GameInProgress)(TTTGameF)(Ttt.XOPiece)(RDB)

class user login password =
object
  val login = login
  val password = password
  val mutable logged_in = true

  method get_games =
    TTTGames.get_current_games login

end
