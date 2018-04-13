open Ttt_common_lib_types
open Base_types

module type CHALLENGE_STORE =
  sig
    type t
    val create : unit -> t
    val add : t -> challenge -> unit
    val get_all : t -> challenge list
    val remove_id : t -> id -> Base_types.remove_challenge
    val remove_user_challenges : t -> string -> unit
  end

module Make(ChallengeStore : CHALLENGE_STORE) =
  struct

    type t = {
        mutex: Lwt_mutex.t;
        store: ChallengeStore.t;
      }

    let create () =
      {
        mutex = Lwt_mutex.create ();
        store = ChallengeStore.create ();
      }

    let add_challenge challenges challenge =
      Logs.debug (fun m -> m "CS add challenge");
      let open Lwt in
      Lwt_mutex.lock challenges.mutex >>=
        (fun () ->
          ChallengeStore.add challenges.store challenge;
          Lwt_mutex.unlock challenges.mutex;
          Lwt.return ();
        )

    let public_challenges_for_user challenges user =
      let filter_function challenge =
        match Ttt_server_lib_challenge.opponent challenge with
        | None ->
           let challenger = Ttt_server_lib_challenge.challenger challenge in
           user <> challenger
        | _ -> false
      in

      let publics =
        List.filter filter_function
                    (ChallengeStore.get_all challenges.store)
      in
      publics

    let private_challenges_for_user challenges user =
      let filter_function challenge =
        match Ttt_server_lib_challenge.opponent challenge with
          Some(opp) -> user = opp
        | _ -> false
      in
      let privates =
        List.filter filter_function
                    (ChallengeStore.get_all challenges.store)
      in
      privates

    let remove_by_id challenges id =
      let open Ttt_server_lib_types in
      let open Lwt in
      Lwt_mutex.lock challenges.mutex >|=
        (fun () ->
          let result =
            ChallengeStore.remove_id challenges.store id in
          Lwt_mutex.unlock challenges.mutex;
          result
        )

    let purge_user_challenges challenges user =
      let open Lwt in
      Lwt_mutex.lock challenges.mutex >|=
        (fun () ->
          ChallengeStore.remove_user_challenges
            challenges.store
            user;
          Lwt_mutex.unlock challenges.mutex
        )
end
