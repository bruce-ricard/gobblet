type game_name =
  [
  | `TicTacToeClassical
  | `TicTacToeXOnly
  ]

type frontend_challenge = {
    id: int;
    challenger: string;
    game_type: game_name option;
  }

class id (n : int) =
object
  val n = n
  method get_id = n
end
