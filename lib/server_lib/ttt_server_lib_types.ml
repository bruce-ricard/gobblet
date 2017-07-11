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

module type GAME_DB = functor
    (Game : sig type t end) ->
  sig
    val put : id -> Game.t -> unit
    val get : id -> Game.t
  end


(*module type FB_REACT_GAME =
  functor (GameInProgress : GAME_IN_PROGRESS) (Game : GAME) (Piece : PIECE) (ReactDB : REACT_DB) ->
  sig
    type t
    val new_game : (player -> string) -> t * GameInProgress(Game)(Piece).t React.E.t
    val move : t -> row:int -> column:int -> string -> move_result
    val piece_at : t -> row:int -> column:int -> Piece.t option
    val username_and_piece : t -> player -> (string * Piece.t)
    val user_status : t -> string -> user_action
  end*)

module type GAMES =
  functor
            (GameInProgress : GAME_IN_PROGRESS)
              (Piece : PIECE) (ReactDB : REACT_DB) ->
  sig
    val new_game : string -> string -> id * GameInProgress(Piece).t React.E.t
    val move : id -> row:int -> column:int -> string -> move_result
    val piece_at : id -> row:int -> column:int -> Piece.t option React.event
    val username_and_piece : id -> player -> (string * Piece.t)
    (*    val user_status : id -> string -> user_action*)
    val game_status : id -> game_in_progress_status React.event
    val refresh_game : id -> unit

    val get_current_games : string -> (id * GameInProgress(Piece).t React.E.t) list
  val get_react_game_by_id : id -> GameInProgress(Piece).t React.E.t option
(*    val get_game_by_id : id -> GameInProgress(Piece).t option
    val get_finished_games : string -> GameInProgress.t list
    val get_challenges_sent : string -> GameInProgress.t list
    val get_challenges_received : string -> GameInProgress.t list*)
  end
