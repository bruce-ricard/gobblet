open Ttt_game_lib_types
open Ttt_common_lib_types

module type REACT_DB = functor
    (Game : sig type t end) ->
  sig
    val put : id -> Game.t React.event * (?step:React.step -> Game.t -> unit) -> unit
    val delete : id -> unit
    val get_channel : id -> Game.t React.event option (* Make this a "down" react already, since it's only for frontend use *)
    val get_update_function : id -> (?step:React.step -> Game.t -> unit)
  end

type remove_challenge =
  | Id_not_present
  | Deleted of string

module type CHALLENGE_DB =
  sig
    type t
    val load : unit -> t
    val create : t -> string -> ?opponent:string -> id -> unit
    val public_challenges_for_user : t -> string -> (id * string) list
    val private_challenges_for_user : t -> string -> (id * string) list
    val event_listener : t -> unit React.event
    val remove : t -> id -> remove_challenge
    val lock : t -> < unlock : unit > Lwt.t
  end

module type CHALLENGE_REACT_DB =
  sig
    val create : id -> unit React.event
    val accept : id -> unit
  end

module type GAME_DB =
  sig
    type game
           (* TODO : remove the users from this function
they can be found from the game. Maybe add a get_players : (string * string)
            function to Game *)
    val put_game : id -> string -> string -> game -> unit
    val get_game : id -> game option
    val delete_game : id -> unit

    val get_games_for_user : string -> (id * string) list
  end

module type GAME_ARCHIVE_DB =
  sig
    type game
    val put_game : id -> game -> unit
    val get_game : id -> game option
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

type 'a extended_event =
  {
    event: 'a React.event;
    initial_data: 'a
  }

module type GAMES =
  sig
    type game
    type piece
    val new_challenge : ?opponent:string -> string -> challenge_result
    val accept_challenge : id -> string -> bool
    val move : id -> row:int -> column:int -> string -> move_result
    val piece_at : id -> row:int -> column:int -> piece option React.event
    val username_and_piece : id -> player -> (string * piece)
    (*    val user_status : id -> string -> user_action*)
    val game_status : id -> game_in_progress_status React.event
    val refresh_game : id -> unit

    val get_current_games : string -> (id * string) list
    val get_react_game_by_id : id -> game React.E.t option

    val get_private_challenges : string -> (int * string) list extended_event
    val get_public_challenges : string -> (int * string) list extended_event

(*    val get_game_by_id : id -> GameInProgress(Piece).t option
    val get_finished_games : string -> GameInProgress.t list
    val get_challenges_sent : string -> GameInProgress.t list
    val get_challenges_received : string -> GameInProgress.t list*)
  end
