open OUnit2
open Ttt_game_lib_games
open Ttt_game_lib_types

let test_place_a_piece _ =
  let board = Boards.XOWinBoard.empty_board () in
  let x : Ttt_game_lib_pieces.XOPiece.t = `X in
  assert_equal
    (`Ok `KeepPlaying)
    (Boards.XOWinBoard.place board {row=1;column=1} x)

let test_invalid_move _ =
  let board = Boards.XOWinBoard.empty_board () in
  let place = Boards.XOWinBoard.place board in
  assert_equal
    (`Ok `KeepPlaying)
    (place {row = 0; column = 0} `X);
  assert_equal
    (`Invalid `InvalidMove)
    (place {row = 0; column = 0} `X)

let test_win _ =
  let board = Boards.XOWinBoard.empty_board () in
  let place = Boards.XOWinBoard.place board in
  assert_equal
    (`Ok `KeepPlaying)
    (place {row = 0; column = 0} `X);
  assert_equal
    (`Ok `KeepPlaying)
    (place {row = 0; column = 1} `X);
  assert_equal
    (`Ok `Won)
    (place {row = 0; column = 2} `X)


let suite =
  "game_lib" >:::
    [
      "place a piece" >:: test_place_a_piece;
      "test invalid move" >:: test_invalid_move;
      "win test" >:: test_win;
    ]

let () =
  run_test_tt_main suite
