module type DAO =
  sig
    open Ttt_common_lib_types

    val get_rating : game_name -> string -> rating option
    val set_rating :
      game_name -> string -> Ttt_common_lib_types.rating -> bool
  end

module MockDao =
  struct
    let get_rating _ _ =
      Logs.err (fun m -> m "Mock getting rating");
      None

    let set_rating _ _ _ =
      Logs.err (fun m -> m "Mock setting rating");
      true
  end
module Make
         (Dao : DAO)
         (Archive : Internal_types.ARCHIVE)
       : Ttt_server_lib_types.GAME_LIST =
  struct
    module TttcRatingUpdater =
      Rating_updater.Make(
          Ratings.Make
            (Dao)
            (struct let game () = `TicTacToeClassical end)
        )
    module TttXoRatingUpdater =
      Rating_updater.Make(
          Ratings.Make
            (MockDao)
            (struct let game () = `TicTacToeXOnly end)
        )

    module ThreeMenMorrisRatingUpdater =
      Rating_updater.Make(
          Ratings.Make
            (Dao)
            (struct let game () = `ThreeMenMorris end)
        )

    module TTTCReporter =
      Reporter.Make(TttcRatingUpdater)(Archive)
    module TTTXoReporter =
      Reporter.Make(TttXoRatingUpdater)(Archive)
    module ThreeMenMorrisReporter =
      Reporter.Make(ThreeMenMorrisRatingUpdater)(Archive)

    module TTTCI =
      Ttt_game_lib_games.TicTacToeClassical(TTTCReporter)
    module TTTXOI =
      Ttt_game_lib_games.TicTacToeXOnly(TTTXoReporter)
    module ThreeMenMorrisInternal =
      Ttt_game_lib_games.ThreeMenMorris(ThreeMenMorrisReporter)

    module TicTacToeClassical =
      Ttt_server_lib_game_api.Make(TTTCI)

    module TicTacToeXOnly =
      Ttt_server_lib_game_api.Make(TTTXOI)

    module ThreeMenMorris =
      Ttt_server_lib_game_api.Make(ThreeMenMorrisInternal)
  end
