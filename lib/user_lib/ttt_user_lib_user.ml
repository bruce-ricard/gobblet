(*open Ttt_lib_types*)


class user login password =
object
  val login = login
  val password = password
  val mutable logged_in = true

  method get_games = ([] : int list)
    (* TTTGames.get_current_games login *)

end
