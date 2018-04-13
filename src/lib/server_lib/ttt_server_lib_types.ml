open Ttt_common_lib_types
open Ttt_game_lib_types

include Base_types

module type GAME_ID_GENERATOR =
  sig
    val next : unit -> id
  end

module FrontendBackendGame =
  struct
    type 'a fb_game = {
        game : 'a;
        event : 'a React.event;
        update : ?step:React.step -> 'a -> unit
      }
  end

open FrontendBackendGame

type challenge_accepted =
  {
    id: id;
    game_name: game_name option;
    challenger: string;
    chalengee: string;
  }

type attempt_accepting_challenge =
  | Accept of challenge_accepted
  | Declined

type create_challenge_result =
  | Challenge_created of id * (unit React.event)
  | Challenge_accepted of challenge_accepted
  | Error of string

module type CHALLENGE_API =
  sig
    type t
    val load : unit -> t
    val new_challenge : t -> string -> ?opponent:string -> game_name option
                 -> create_challenge_result Lwt.t
    val accept : t -> id -> string -> attempt_accepting_challenge Lwt.t
    val public_challenges_for_user :
      t -> string -> frontend_challenge list React.event
    val private_challenges_for_user :
      t -> string -> frontend_challenge list React.event
  end

module type GAME =
  sig
    type piece
    type game
    val new_game : (player -> string) -> game
    val move : game -> row:int -> column:int -> string -> move_result
    val piece_at : game -> row:int -> column:int -> piece option React.event
    val username_and_piece : game -> player -> (string * piece)
    val game_status : game -> game_in_progress_status React.event
    val refresh_game : game -> unit
  end

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
    val archive_game : id -> unit
  end

module type RATING_UPDATER =
  sig
    open Ttt_game_lib_types
    open Ttt_common_lib_types
    val update_ratings_from_result: report_result -> id -> unit
  end

module type GAMES =
  sig
    val new_challenge : ?opponent:string -> string -> game_name option
                        -> create_challenge_result Lwt.t
    val accept_challenge : id -> string -> bool Lwt.t

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

module type GAME_API =
  sig
    type piece
    type game
    val new_game :
      (Ttt_game_lib_types.player -> string) ->
      Ttt_common_lib_types.id -> game
    val place :
      game ->
      row:int -> column:int -> string -> Ttt_game_lib_types.move_result
    val move :
      game ->
      Ttt_game_lib_types.move -> string -> Ttt_game_lib_types.move_result
    val username_and_piece :
      game ->
      Ttt_game_lib_types.player -> string * piece
    val piece_at :
      game ->
      row:int -> column:int -> piece option React.event
    val game_status :
      game ->
      Ttt_game_lib_types.game_in_progress_status React.event
    val refresh_game : game -> unit
  end
