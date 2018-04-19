open Ttt_common_lib_types

type challenge = Ttt_server_lib_challenge.t

type remove_challenge =
  | Id_not_present
  | Deleted of challenge

type 'a fb_game = {
    game : 'a;
    event : 'a React.event;
    update : ?step:React.step -> 'a -> unit
  }

type 'a create_challenge_result =
  | Challenge_created of id * (unit React.event)
  | Challenge_accepted of 'a
  | Error of string

module GameTypes =
  struct
    open Ttt_game_lib_games
    type tttc = GameInProgressTypes.tic_tac_toe_classical fb_game
    type tttxo = GameInProgressTypes.tic_tac_toe_x_only fb_game
    type three_men_morris = GameInProgressTypes.three_men_morris fb_game

    type named_game =
      [
      | `TicTacToeClassical of tttc
      | `TicTacToeXOnly of tttxo
      | `ThreeMenMorris of three_men_morris
      ]
  end
