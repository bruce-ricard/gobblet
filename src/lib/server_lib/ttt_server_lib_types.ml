open Ttt_common_lib_types
open Ttt_game_lib_types

module type GAME_ID_GENERATOR =
  sig
    val next : unit -> id
  end

include Internal_types

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

module type GAME_DB =
  sig
    (* TODO : remove the users from this function
they can be found from the game. Maybe add a get_players : (string * string)
            function to Game *)
    open GameTypes

    val put_game : id -> string -> string -> named_game -> unit
    val get_game : id -> named_game option
    val delete_game : id -> unit

    val get_games_for_user : string -> (id * string) list
  end

module type ARCHIVE =
  sig
    val archive : id -> unit
  end

type challenge_result =
  | Challenge_created of id * (unit React.event)
  | Challenge_accepted of id
  | Error of string

module type GAMES =
  sig
    val new_challenge : ?opponent:string -> string -> game_name option
                        -> challenge_result
    val accept_challenge : id -> string -> bool

    val get_current_games : string -> (id * string) list
    val get_private_challenges : string -> frontend_challenge list React.event
    val get_public_challenges : string -> frontend_challenge list React.event

    val get_game : id -> GameTypes.named_game option
  end

module type GAME_ARCHIVE_DB =
  sig
    val put_game : id -> GameTypes.named_game -> unit
    val get_game : id -> GameTypes.named_game option
    val get_games_for_user : string -> (id * string) list
  end
