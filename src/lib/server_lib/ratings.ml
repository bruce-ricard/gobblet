type user = string

module type RATINGS_BY_GAME =
  sig
    open Ttt_common_lib_types

    val get_rating: game_name ->
                    user ->
                    rating option
    val set_rating: game_name ->
                    user ->
                    rating ->
                    bool
  end

open Ttt_common_lib_types

module Make(RatingsByGame : RATINGS_BY_GAME)
           (Game : sig val game : unit -> game_name end) =
  struct
    let get_rating user =
      Logs.debug (fun m -> m "Getting rating for %s" user);
      RatingsByGame.get_rating (Game.game ()) user

    let set_rating user rating =
      Logs.debug (fun m -> m "Setting rating for %s to %f"
                             user rating.rating);
      RatingsByGame.set_rating (Game.game ()) user rating
  end
