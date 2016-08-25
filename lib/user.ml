class user login password =
object
  val login = login
  val password = password
  val mutable logged_in = true
  val mutable games = []

  method start_game (g : int) =
    games <- g :: games

  method get_games =
    games

end
