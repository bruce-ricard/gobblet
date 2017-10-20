open Ttt_game_lib_types
open Ttt_common_lib_types

type challenge = Ttt_server_lib_challenge.t

type remove_challenge =
      | Id_not_present
      | Deleted of challenge

module type CHALLENGES =
  sig
    type t
    val load : unit -> t
    val create : t -> string -> ?opponent:string -> game_name option -> id
                 -> challenge
    val public_challenges_for_user : t -> string -> challenge list
    val private_challenges_for_user : t -> string -> challenge list
    val remove : t -> id -> remove_challenge
    val lock : t -> < unlock : unit > Lwt.t
    val event_listener : t -> unit React.event
    val send_updates : t -> unit
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
