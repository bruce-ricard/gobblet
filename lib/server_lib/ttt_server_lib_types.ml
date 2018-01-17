open Ttt_common_lib_types
open Ttt_game_lib_types

module type GAME_ID_GENERATOR =
  sig
    val next : unit -> id
  end

include Internal_types

type ('a,'b) named_game =
  [
  | `TicTacToeClassical of 'a
  | `TicTacToeXOnly of 'b
  ]

type ('a, 'b) named_api_game = ('a fb_game, 'b fb_game) named_game

module type GAME_DB =
  sig
    (* TODO : remove the users from this function
they can be found from the game. Maybe add a get_players : (string * string)
            function to Game *)
    type tttc
    type tttxo

    type ngame = (tttc, tttxo) named_game

    val put_game : id -> string -> string -> ngame -> unit
    val get_game : id -> ngame option
    val delete_game : id -> unit

    val get_games_for_user : string -> (id * string) list
  end

type challenge_result =
  | Challenge_created of id * (unit React.event)
  | Challenge_accepted of id
  | Error of string

module type GAMES =
  sig
    type tttc
    type tttxo

    type ngame = (tttc, tttxo) named_game

    val new_challenge : ?opponent:string -> string -> game_name option
                        -> challenge_result
    val accept_challenge : id -> string -> bool

    val get_current_games : string -> (id * string) list
    val get_private_challenges : string -> frontend_challenge list React.event
    val get_public_challenges : string -> frontend_challenge list React.event

    val get_game : id -> ngame option
  end

module type GAME_ARCHIVE_DB =
  sig
    type tttc
    type tttxo

    type ngame = (tttc, tttxo) named_game

    val put_game : id -> ngame -> unit
    val get_game : id -> ngame option
    val get_games_for_user : string -> (id * string) list
  end
