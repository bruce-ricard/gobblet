open Ttt_game_lib_types

let poly_to_string = function
  | `Draw -> "Draw"
  | `Drawn -> "Drawn"
  | `GameOver -> "GameOver"
  | `GameWasOver -> "GameWasOver"
  | `Invalid -> "Invalid"
  | `InvalidMove -> "InvalidMove"
  | `KeepPlaying -> "KeepPlaying"
  | `Lost -> "Lost"
  | `NotYourTurn -> "NotYourTurn"
  | `O -> "O"
  | `Ok -> "Ok"
  | `Password -> "Password"
  | `Play -> "Play"
  | `PlayOn -> "PlayOn"
  | `Quick -> "Quick"
  | `Submit -> "Submit"
  | `Text -> "Text"
  | `ThreeMenMorris -> "ThreeMenMorris"
  | `TicTacToeClassical -> "TicTacToeClassical"
  | `TicTacToeXOnly -> "TicTacToeXOnly"
  | `Wait -> "Wait"
  | `Watch -> "Watch"
  | `Won -> "Won"
  | `WrongPlayer -> "WrongPlayer"
  | `X -> "X"

let board_move_result_to_string : board_move_result -> string = function
  | `Ok x ->
     Printf.sprintf "%s(%s)"
                    (poly_to_string `Ok)
                    (poly_to_string x)
  | `Invalid x ->
     Printf.sprintf "%s(%s)"
                    (poly_to_string `Invalid)
                    (poly_to_string x)

let board_move_result =
  Alcotest.testable (Fmt.of_to_string board_move_result_to_string) (=)
