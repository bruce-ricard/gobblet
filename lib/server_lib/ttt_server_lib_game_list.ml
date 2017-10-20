module TrivialReporter =
  struct
    type t = unit
    let get () = ()
    let report_game_end () _ =
      Logs.info (fun m -> m "Reporting end of game !!");
      ()
  end
(*
module Ratings(Game : sig
             val game_name : unit -> Ttt_common_lib_types.game_name
           end) =
  struct
    let get_rating user =
      UsersPostgresDao.get_rating user (Game.game_name ())
  end
 *)
module TTTClassicaRatings =
  Ratings.Make(UsersPostgresDao)
    (struct let game () = `TicTacToeClassical end)

module TTTCReporter = Reporter.Make(TTTClassicaRatings)

module TTTCI = Ttt_game_lib_games.TicTacToeClassical(TTTCReporter)
module TTTXOI = Ttt_game_lib_games.TicTacToeXOnly(TrivialReporter)

module TicTacToeClassical =
  Ttt_server_lib_game_api.Make(TTTCI)

module TicTacToeXOnly  =
  Ttt_server_lib_game_api.Make(TTTXOI)
