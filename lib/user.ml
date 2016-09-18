open Game
open Games
open GameInProgress
open Types

module RDB (T : sig type t end) =
  struct
    let games = ref []

    let put id t = games := (id,t) :: !games
    let delete id = games := List.filter (fun (x,_) -> x <> id) !games

    let get id = List.assoc id !games
    let get_object id = fst (get id)
    let get_update_function id = snd (get id)
  end


module Games = MemoryGames
module TTTGameF = Game(Ttt.Board)
module TTTGameInProgress = GameInProgress(TTTGameF)(Ttt.XOPiece)
module TTTGames = Games(FrontAndBackendReactGame.FrontAndBackendReactGame)(GameInProgress)(TTTGameF)(Ttt.XOPiece)(RDB)

class user login password =
object
  val login = login
  val password = password
  val mutable logged_in = true

  method get_games =
    TTTGames.get_current_games login

end
