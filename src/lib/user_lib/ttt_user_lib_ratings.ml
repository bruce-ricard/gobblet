module Glicko2 = Glicko2.Default.SingleGame
module Common = Ttt_common_lib_types

let to_glicko_player =
  let open Common in
  function {rating; rating_deviation; sigma} ->
           Glicko2.{rating; rating_deviation; volatility = sigma}

let from_glicko_player : 'a -> Ttt_common_lib_types.rating =
  let open Glicko2 in
  function {rating; rating_deviation; volatility} ->
           Common.{rating; rating_deviation; sigma = volatility}

let new_player () =
  match Glicko2.default_player () with
  | `Ok p -> `Ok (from_glicko_player p)
  | `Error e -> `Error
                 (e : [ `UnknownError of string
                      | `InvalidArgument of string
                 ])

type game_result =
  {
    player1: Common.rating;
    player2: Common.rating;
    game_outcome: [ `Player1Win | `Player2Win | `Draw ];
  }

let game_result_to_glicko {player1; player2; game_outcome} =
  Glicko2.{
          player1 = to_glicko_player player1;
          player2 = to_glicko_player player2;
          game_outcome
  }

type new_ratings =
  {
    new_player1: Common.rating;
    new_player2: Common.rating;
  }

let from_glicko_new_ratings new_ratings =
  let new_player1 =
    from_glicko_player new_ratings.Glicko2.new_player1
  and new_player2 =
    from_glicko_player new_ratings.Glicko2.new_player2
  in
  { new_player1; new_player2; }

let from_glicko_rate_result = function
  | `Ok result -> `Ok (from_glicko_new_ratings result)
  | `Error e -> `Error
                 (e : [ `InvalidVolatility
                      | `ExceededIterations
                      | `UnknownError of string
                 ])

let rate (result : game_result) =
  from_glicko_rate_result (Glicko2.rate (game_result_to_glicko result))
