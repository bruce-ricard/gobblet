open Types

module RDB : REACT_DB = functor (T : sig type t end) ->
  struct
    let objects = ref []

    let put id t = objects := (id,t) :: !objects
    let delete id = objects := List.filter (fun (x,_) -> x <> id) !objects

    let get id = List.assoc id !objects
    let get_channel id = fst (get id)
    let get_update_function id = snd (get id)
  end

(*
module TTTGameF = Game.Make(Ttt.Board)
module TTTGameInProgress = GameInProgress.Make(TTTGameF)(Ttt.XOPiece)
module TTTFBGame = FrontAndBackendReactGame.Make(GameInProgress.Make)(TTTGameF)(Ttt.XOPiece)(RDB)
module TTTGames = Games.Make(FrontAndBackendReactGame.Make)(GameInProgress)(TTTGameF)(Ttt.XOPiece)(RDB)
 *)

module TTTGameF = Game.Make(Ttt.Board)
module TTT = Export.Make(GameInProgress.Make)(TTTGameF)(Ttt.XOPiece)(RDB)

class user login password =
object
  val login = login
  val password = password
  val mutable logged_in = true

  method get_games = ([] : int list)
    (* TTTGames.get_current_games login *)

end
