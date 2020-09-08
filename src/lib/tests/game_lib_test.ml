open Ttt_game_lib_games
open Ttt_game_lib_types
open Checkers

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
    "should be an illegal move"
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
    "should be a legal move 1"
    (`Ok `KeepPlaying)
    (place {row=0;column=0} `X);

    Alcotest.check
    board_move_result
    "should be a legal move 2"
    (`Ok `KeepPlaying)
    (place {row=0;column=1} `X);

    Alcotest.check
    board_move_result
    "should win the game"
    (`Ok `Won)
    (place {row=0;column=2} `X)

let board_suite = [
    "place a piece", `Quick, test_place_a_piece;
    "test invalid move", `Quick, test_invalid_move;
    "test place piece again", `Quick, test_invalid_move_twice;
    "win test", `Quick, test_win;
  ]
