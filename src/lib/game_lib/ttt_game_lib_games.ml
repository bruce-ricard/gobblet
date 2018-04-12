open Boards

module Classical = Game_internal.Make(XOWinBoard)
module ClassicalLose = Game_internal.Make(XOLoseBoard)
module Xonly = Game_internal.Make(XWinBoard)
module ThreeMorrisInternal = Game_internal.Make(ThreeMorrisBoard)

module TicTacToeClassicalF = Game_in_progress.Make(Classical)
module TicTacToeXOnlyF = Game_in_progress.Make(Xonly)
module ThreeMenMorrisF = Game_in_progress.Make(ThreeMorrisInternal)

module TicTacToeClassical = TicTacToeClassicalF.Make
module TicTacToeXOnly = TicTacToeXOnlyF.Make
module ThreeMenMorris = ThreeMenMorrisF.Make

module GameInProgressTypes =
  struct
    type tic_tac_toe_classical = TicTacToeClassicalF.t
    type tic_tac_toe_x_only = TicTacToeXOnlyF.t
    type three_men_morris = ThreeMenMorrisF.t
  end
