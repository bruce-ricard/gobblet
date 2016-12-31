open Types

module RDB : REACT_DB = functor (T : sig type t end) ->
  struct
    let objects = ref []

    let put id t = objects := (id,t) :: !objects
    let delete id = objects := List.filter (fun (x,_) -> x <> id) !objects

    let get id =
       (List.assoc id !objects)
    let get_channel id = fst (get id)
    let get_update_function id = snd (get id)
  end

module WW =
  struct
    let wins = true
  end

module WL =
  struct
    let wins = false
  end

module WBoard = Ttt.Board(WW)
module LBoard = Ttt.Board(WL)

module WTTTGameF = Game.Make(WBoard)
module LTTTGameF = Game.Make(LBoard)
module TTT = Export.Make(GameInProgress.Make)(WTTTGameF)(Pieces.XOPiece)(RDB)
module TTTXonly = Export.Make(GameInProgress.Make)(LTTTGameF)(Pieces.XPiece)(RDB)

class user login password =
object
  val login = login
  val password = password
  val mutable logged_in = true

  method get_games = ([] : int list)
    (* TTTGames.get_current_games login *)

end
