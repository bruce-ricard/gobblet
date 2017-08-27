module WW =
  struct
    let wins = true
  end

module WL =
  struct
    let wins = false
  end

module WBoard = Tictactoe_board.Make(WW)(Ttt_game_lib_pieces.XOPiece)
module LBoard = Tictactoe_board.Make(WL)(Ttt_game_lib_pieces.XOPiece)

module WTTTGameF = Game_internal.Make(WBoard)
module LTTTGameF = Game_internal.Make(LBoard)

module TicTacToeClassical = Game_in_progress.Make(WTTTGameF)
module TicTacToeXonly = Game_in_progress.Make(WTTTGameF)
