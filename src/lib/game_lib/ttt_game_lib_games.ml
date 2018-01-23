module WW =
  struct
    let wins = true
  end

module WL =
  struct
    let wins = false
  end

module XOWinBoard = Tictactoe_board.Make(WW)(Ttt_game_lib_pieces.XOPiece)
module XOLoseBoard = Tictactoe_board.Make(WL)(Ttt_game_lib_pieces.XOPiece)

module XWinBoard = Tictactoe_board.Make(WW)(Ttt_game_lib_pieces.XPiece)
module XLoseBoard = Tictactoe_board.Make(WL)(Ttt_game_lib_pieces.XPiece)

module Classical = Game_internal.Make(XOWinBoard)
module ClassicalLose = Game_internal.Make(XOLoseBoard)
module Xonly = Game_internal.Make(XWinBoard)

module TicTacToeClassical = Game_in_progress.Make(Classical)
module TicTacToeXOnly = Game_in_progress.Make(Xonly)
