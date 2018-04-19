open Internal_types

module Make
         (RatingUpdater : RATING_UPDATER)
         (Archive : ARCHIVE)
  =
  struct
    type t = unit
    let get () = ()

    let report_game_end () result id =
      Logs.debug (fun m -> m "Game end reported");
      RatingUpdater.update_ratings_from_result result id;
      Archive.archive_game id
  end
