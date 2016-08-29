open Game
open Games

module Games = MemoryGames
module TTTGame = Game(Ttt.Board)(Ttt.XOPiece)
module TTTGames = Games(TTTGame)

class user login password =
object
  val login = login
  val password = password
  val mutable logged_in = true

  method get_games =
    TTTGames.get_current_games login

end
