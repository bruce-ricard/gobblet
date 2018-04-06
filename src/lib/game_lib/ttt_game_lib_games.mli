open Ttt_common_lib_types
open Ttt_game_lib_types

module GameInProgressTypes :
  sig
    type tic_tac_toe_classical
    type tic_tac_toe_x_only
    type three_men_morris
  end

module TicTacToeClassical : functor
  (Reporter : REPORTER) ->
sig
  type t = GameInProgressTypes.tic_tac_toe_classical
  type piece = [`X | `O]
  val new_game : (player -> string) -> id -> t
  val place : t -> square -> string -> move_result
  val piece_at : t -> row:int -> column:int -> piece option
  val move : t -> move -> string -> move_result
  val username_and_piece : t -> player -> (string * piece)
  val game_status : t -> game_in_progress_status
  val store : t -> serialized_game
  val restore : serialized_game -> t option
end

module TicTacToeXOnly : functor
  (Reporter : REPORTER) ->
sig
  type t = GameInProgressTypes.tic_tac_toe_x_only
  type piece = [`X]
  val new_game : (player -> string) -> id -> t
  val place : t -> square -> string -> move_result
  val move : t -> move -> string -> move_result
  val piece_at : t -> row:int -> column:int -> piece option
  val username_and_piece : t -> player -> (string * piece)
  val game_status : t -> game_in_progress_status
  val store : t -> serialized_game
  val restore : serialized_game -> t option
end

module ThreeMenMorris : functor
  (Reporter : REPORTER) ->
sig
  type t = GameInProgressTypes.three_men_morris
  type piece = [`X | `O]
  val new_game : (player -> string) -> id -> t
  val place : t -> square -> string -> move_result
  val move : t -> move -> string -> move_result
  val piece_at : t -> row:int -> column:int -> piece option
  val username_and_piece : t -> player -> (string * piece)
  val game_status : t -> game_in_progress_status
  val store : t -> serialized_game
  val restore : serialized_game -> t option
end
