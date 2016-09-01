open Game
open Games

module Games = MemoryGames
module TTTGameF = Game(Ttt.Board)
module TTTGame = TTTGameF(Ttt.XOPiece)
module TTTGames = Games(TTTGameF)(Ttt.XOPiece)

class user login password =
object
  val login = login
  val password = password
  val mutable logged_in = true

  method get_games =
    TTTGames.get_current_games login

end
