module Common = Ttt_common_lib_types

val new_player : unit ->
                 [
                 | `Ok of Ttt_common_lib_types.rating
                 | `Error of [
                   | `InvalidArgument of string
                   | `UnknownError of string
                   ]
                 ]

type game_result =
  {
    player1: Common.rating;
    player2: Common.rating;
    game_outcome: [ `Player1Win | `Player2Win | `Draw ];
  }

type new_ratings =
  {
    new_player1: Common.rating;
    new_player2: Common.rating;
  }

val rate : game_result ->
           [
           | `Ok of new_ratings
           | `Error of
               [
               | `ExceededIterations
               | `InvalidVolatility
               | `UnknownError of string
               ]
           ]
