module WW =
  struct
    let wins = true
  end

module WL =
  struct
    let wins = false
  end

module TttMoveDecider =
  struct
    let is_place_allowed semi_move = semi_move <= 9
    let is_move_allowed _ = false
  end

module ThreeMorrisMoveDecider =
  struct
    let is_place_allowed semi_move = semi_move <= 5
    let is_move_allowed semi_move = semi_move >= 6
  end

module XOWinBoard =
  Tictactoe_board.Make
    (WW)
    (Ttt_game_lib_pieces.XOPiece)
    (TttMoveDecider)

module XOLoseBoard =
  Tictactoe_board.Make
    (WL)
    (Ttt_game_lib_pieces.XOPiece)
    (TttMoveDecider)

module XWinBoard =
  Tictactoe_board.Make
    (WW)
    (Ttt_game_lib_pieces.XPiece)
    (TttMoveDecider)

module XLoseBoard =
  Tictactoe_board.Make
    (WL)
    (Ttt_game_lib_pieces.XPiece)
    (TttMoveDecider)

module ThreeMorrisBoard =
  Tictactoe_board.Make
    (WW)
    (Ttt_game_lib_pieces.XOPiece)
    (ThreeMorrisMoveDecider)

module Classical = Game_internal.Make(XOWinBoard)
module ClassicalLose = Game_internal.Make(XOLoseBoard)
module Xonly = Game_internal.Make(XWinBoard)
module ThreeMorrisInternal = Game_internal.Make(ThreeMorrisBoard)

module TicTacToeClassical = Game_in_progress.Make(Classical)
module TicTacToeXOnly = Game_in_progress.Make(Xonly)
module ThreeMenMorris = Game_in_progress.Make(ThreeMorrisInternal)
