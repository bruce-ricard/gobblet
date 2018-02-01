open Ttt_common_lib_types

type user = string

module type RATINGS =
  sig
    val get_rating : user -> rating option
    val set_rating : user -> rating -> bool
  end

module Make(Ratings : RATINGS) =
  struct
    type t = unit
    let get () = ()

    let to_glicko_player =
      let open Ttt_game_lib_types in
      function {rating; rating_deviation; sigma} ->
        let open Glicko2 in
        {rating; rating_deviation; volatility = sigma}

    let from_glicko_player : 'a -> Ttt_common_lib_types.rating =
      let open Glicko2 in
      function {rating; rating_deviation; volatility} ->
        {rating; rating_deviation; sigma = volatility}

    let initial_rating () =
      let open Ttt_game_lib_types in
      {rating = 1500.; rating_deviation = 350.; sigma = 0.06}

    let get_rating user =
      match Ratings.get_rating user with
      | Some rating -> rating
      | None -> initial_rating ()

    type 'a tt = {
        result : 'a;
        player1 : string;
        player2 : string;
      }

    let create_draw_result player1 player2 =
      let p1rating = get_rating player1
      and p2rating = get_rating player2 in
      let open Glicko2 in
      let result = {
        player1 = to_glicko_player p1rating;
        player2 = to_glicko_player p2rating;
        game_outcome = Draw;
        }
      in
      {
        result;
        player1;
        player2
      }

    let create_win_result ~winner ~loser =
      let p1rating = get_rating winner
      and p2rating = get_rating loser in
      let open Glicko2 in
      let result = {
          player1 = to_glicko_player p1rating;
          player2 = to_glicko_player p2rating;
          game_outcome = Player1Win;
        }
      in
      {
        result;
        player1 = winner;
        player2 = loser;
      }

    let compute_rate_result result =
      let open Ttt_game_lib_types in
      Logs.debug (fun m -> m "computing rate result");
      let game_result =
        match result with
        | Draw {player1; player2} ->
           create_draw_result player1 player2
        | Decisive {winner; loser} ->
           create_win_result ~winner ~loser
      in
      Logs.debug (fun m -> m "about to call Glicko2");
      let result = Glicko2.rate game_result.result in
      Logs.debug (fun m -> m "called Glicko2");
      {
        result;
        player1 = game_result.player1;
        player2 = game_result.player2;
      }

    let report_game_end () result =
      Logs.debug (fun m -> m "Game end reported");
      let rate_result = compute_rate_result result in
      Logs.debug (fun m -> m "rate result computed");
      match rate_result.result with
        Glicko2.NewRatings(new_ratings) ->
        begin
          let newP1rating =
            from_glicko_player
              new_ratings.Glicko2.new_player1
          and newP2rating =
            from_glicko_player
              new_ratings.Glicko2.new_player2
          in
          Logs.debug (fun m -> m "new ratings computed");
          let _ = Ratings.set_rating rate_result.player1 newP1rating in
          Logs.debug (fun m -> m "p1 rating sent");
          let _ = Ratings.set_rating rate_result.player2 newP2rating in
          Logs.debug (fun m -> m "p2 rating sent");
          ()
        end
      | Glicko2.InvalidVolatility ->
         Logs.err (fun m -> m "Invalid Volatility while trying to rate")
      | Glicko2.InternalError ->
         Logs.err (fun m ->
             m "Glicko2 internal error while trying to rate"
           )

  end
