open Ttt_game_lib_types
open Ttt_common_lib_types

open Base_types

module type CHALLENGE_CRITICAL_SECTION =
  sig
    type t
    val create : unit -> t
    val add_challenge : t -> challenge -> unit Lwt.t
    val public_challenges_for_user : t -> string -> challenge list
    val private_challenges_for_user : t -> string -> challenge list
    val remove_by_id : t -> id -> remove_challenge Lwt.t
    val purge_user_challenges : t -> string -> unit Lwt.t
  end
