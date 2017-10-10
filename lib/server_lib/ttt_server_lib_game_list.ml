module TicTacToeClassical =
  Ttt_server_lib_game_api.Make(Ttt_game_lib_games.TicTacToeClassical)

module TicTacToeXOnly  =
  Ttt_server_lib_game_api.Make(Ttt_game_lib_games.TicTacToeXOnly)
