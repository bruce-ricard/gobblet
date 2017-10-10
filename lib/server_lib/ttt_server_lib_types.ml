open Ttt_game_lib_types
open Ttt_common_lib_types

include Internal_types

type named_db_game =
  [
  | `TicTacToeClassical of Ttt_game_lib_games.TicTacToeClassical.t
  | `TicTacToeXOnly of Ttt_game_lib_games.TicTacToeXOnly.t
  ]

type named_api_game =
  [
  | `TicTacToeClassical of Ttt_server_lib_game_list.TicTacToeClassical.game
  | `TicTacToeXOnly of Ttt_server_lib_game_list.TicTacToeXOnly.game
  ]

module type GAME_DB =
  sig
    (* TODO : remove the users from this function
they can be found from the game. Maybe add a get_players : (string * string)
            function to Game *)
    val put_game : id -> string -> string -> named_api_game -> unit
    val get_game : id -> named_api_game option
    val delete_game : id -> unit

    val get_games_for_user : string -> (id * string) list
  end

module type GAME_ARCHIVE_DB =
  sig
    type game
    val put_game : id -> named_db_game -> unit
    val get_game : id -> named_db_game option
    val get_games_for_user : string -> (id * string) list
  end

module type GAME_ID_GENERATOR =
  sig
    val next : unit -> id
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

    val get_game : id -> named_api_game option
  end
