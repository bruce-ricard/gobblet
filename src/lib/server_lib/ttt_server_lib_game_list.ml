module TrivialReporter =
  struct
    type t = unit
    let get () = ()
    let report_game_end () _ _ =
      Logs.err (fun m -> m "Trivial reporting end of game !!");
      ()
  end

module type DAO =
  sig
    open Ttt_common_lib_types

(*    val get : string -> Sha256.t -> bool
    val exists :
      [< `TicTacToeClassical | `TicTacToeXOnly ] ->
      'a -> Ttt_common_lib_types.rating option*)
    val get_rating : game_name -> string -> rating option
    val set_rating :
      game_name -> string -> Ttt_common_lib_types.rating -> bool
  end

module Make
         (Dao : DAO)
         (Archive : Ttt_server_lib_types.ARCHIVE) =
  struct
    module TTTClassicaRatings =
      Ratings.Make(Dao)
        (struct let game () = `TicTacToeClassical end)

    module TTTCReporter = Reporter.Make(TTTClassicaRatings)(Archive)

    module TTTCI =
      Ttt_game_lib_games.TicTacToeClassical(TTTCReporter)
    module TTTXOI =
      Ttt_game_lib_games.TicTacToeXOnly(TrivialReporter)
    module ThreeMenMorrisInternal =
      Ttt_game_lib_games.ThreeMenMorris(TrivialReporter)

    module TicTacToeClassical =
      Ttt_server_lib_game_api.Make(TTTCI)

    module TicTacToeXOnly  =
      Ttt_server_lib_game_api.Make(TTTXOI)

    module ThreeMenMorris =
      Ttt_server_lib_game_api.Make(ThreeMenMorrisInternal)
  end
