type game_name =
  [
  | `TicTacToeClassical
  | `TicTacToeXOnly
  | `ThreeMenMorris
  ]

type frontend_challenge = {
    id: int;
    challenger: string;
    game_type: game_name option;
  }

type rating = {
    rating: float;
    rating_deviation: float;
    sigma: float
  }

class id (n : int) =
object
  val n = n
  method get_id = n
end
