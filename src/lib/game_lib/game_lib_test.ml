open Ttt_game_lib_games
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

let test_place_a_piece () =
  let board = Boards.XOWinBoard.empty_board () in
  let x : Ttt_game_lib_pieces.XOPiece.t = `X in
  Alcotest.check
    board_move_result
    "should be a legal move"
    (`Ok `KeepPlaying)
    (Boards.XOWinBoard.place board {row=1;column=1} x)

let test_invalid_move () =
  let board = Boards.XOWinBoard.empty_board () in
  Alcotest.check
    board_move_result
    "should be a legal move"
    (`Invalid `InvalidMove)
    (Boards.XOWinBoard.place board {row=0;column=3} `X)

let test_invalid_move_twice () =
  let board = Boards.XOWinBoard.empty_board () in
  let place = Boards.XOWinBoard.place board in

  Alcotest.check
    board_move_result
    "should be a legal move"
    (`Ok `KeepPlaying)
    (place {row=0;column=0} `X);

  Alcotest.check
    board_move_result
    "should be a illegal move, a piece is already on this square"
    (`Invalid `InvalidMove)
    (place {row=0;column=0} `X)

let test_win () =
  let board = Boards.XOWinBoard.empty_board () in
  let place = Boards.XOWinBoard.place board in
  Alcotest.check
    board_move_result
    "should be a legal move"
    (`Ok `KeepPlaying)
    (place {row=0;column=0} `X);

    Alcotest.check
    board_move_result
    "should be a legal move"
    (`Ok `KeepPlaying)
    (place {row=0;column=1} `X);

    Alcotest.check
    board_move_result
    "should be a legal move"
    (`Ok `Won)
    (place {row=0;column=2} `X)


let board_suite = [
    "place a piece", `Quick, test_place_a_piece;
    "test invalid move", `Quick, test_invalid_move;
    "test place piece again", `Quick, test_invalid_move_twice;
    "win test", `Quick, test_win;
  ]

let () =
  Alcotest.run
    "game lib"
    [
      "board suite", board_suite
    ]
