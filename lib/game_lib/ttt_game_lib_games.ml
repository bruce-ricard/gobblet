module WW =
  struct
    let wins = true
  end

module WL =
  struct
    let wins = false
  end

module WBoard = Tictactoe_board.Make(WW)
module LBoard = Tictactoe_board.Make(WL)

module WTTTGameF = Game_internal.Make(WBoard)
module LTTTGameF = Game_internal.Make(LBoard)

module TicTacToeClassical = Game_in_progress.Make(WTTTGameF)
module TicTacToeXonly = Game_in_progress.Make(WTTTGameF)
