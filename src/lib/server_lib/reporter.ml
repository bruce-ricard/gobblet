module Make
         (RatingUpdater : Ttt_server_lib_types.RATING_UPDATER)
         (Archive : Ttt_server_lib_types.ARCHIVE)
  =
  struct
    type t = unit
    let get () = ()

    let report_game_end () result id =
      Logs.debug (fun m -> m "Game end reported");
      RatingUpdater.update_ratings_from_result result id;
      Archive.archive_game id
  end
