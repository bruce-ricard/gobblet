open Ttt_game_lib_types
open Ttt_common_lib_types

include Base_types

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

module type CHALLENGE_API =
  sig
    type t
    val load : unit -> t
    val new_challenge : t -> string -> ?opponent:string -> game_name option
                 -> challenge_accepted create_challenge_result Lwt.t
    val accept : t -> id -> string -> attempt_accepting_challenge Lwt.t
    val public_challenges_for_user :
      t -> string -> frontend_challenge list React.event
    val private_challenges_for_user :
      t -> string -> frontend_challenge list React.event
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

module type GAME_ARCHIVE_DB =
  sig
    val put_game : id -> GameTypes.named_game -> unit
    val get_game : id -> GameTypes.named_game option
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
