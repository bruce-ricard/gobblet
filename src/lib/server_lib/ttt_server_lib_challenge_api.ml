open Ttt_common_lib_types
open Ttt_server_lib_types
module Challenge = Ttt_server_lib_challenge

module Make
         (ChallengesCriticalSection : Internal_types.CHALLENGE_CRITICAL_SECTION)
         (Id_generator : Ttt_server_lib_types.GAME_ID_GENERATOR)
       : Internal_types.CHALLENGE_API
  =
  struct

    type t = {
        event_listener: unit React.event;
        trigger_event: unit -> unit;
        mutex: Lwt_mutex.t;
        challenges: ChallengesCriticalSection.t;
      }

    let load () =
      let event_listener, trigger_event = React.E.create () in
      let rec send_updates () =
        let open Lwt in
        Lwt.async (fun () ->
            Lwt_unix.sleep 3. >>=
              (fun () ->
                trigger_event ();
                Lwt.return (send_updates ())
              )
          )
      in
      send_updates ();
      {
        event_listener;
        trigger_event;
        mutex = Lwt_mutex.create ();
        challenges = ChallengesCriticalSection.create ();
      }

    let create db challenger ?opponent game_name id =
      Logs.debug (fun m -> m "In create challenge");
      let challenge =
        Ttt_server_lib_challenge.create
          ?game_name
          challenger
          ?opponent
          id
      in
      Lwt.async (fun () ->
          ChallengesCriticalSection.add_challenge db.challenges challenge
        );
      challenge

    let challenge_to_frontend challenge =
      {
        id = (Challenge.id challenge)#get_id;
        challenger = Challenge.challenger challenge;
        game_type = Challenge.game_name challenge;
      }

    let public_challenges_for_user challenges user =
      let get_challenges () =
        List.map challenge_to_frontend @@
          ChallengesCriticalSection.public_challenges_for_user
            challenges.challenges
            user
      in
      React.E.map get_challenges challenges.event_listener

    let private_challenges_for_user challenges user =
      let get_challenges () =
        List.map challenge_to_frontend @@
          ChallengesCriticalSection.private_challenges_for_user
            challenges.challenges
            user in
      React.E.map get_challenges challenges.event_listener

    let final_game_name challenge game_name =
      match (Challenge.game_name challenge), game_name with
        None, None -> None
      | None, Some(g) | Some(g), None-> Some(g)
      | Some(g), Some(_) ->
         begin
           Logs.err (fun m -> m "Incompatible games in final_game_name");
           Some(g)
         end

    let accept' challenges ?game_name id user =
      Logs.debug (fun m -> m "challenge api accept'");
      let open Lwt in
      ChallengesCriticalSection.remove_by_id
        challenges.challenges
        id >>=
        function
        | Ttt_server_lib_types.Deleted(challenge) ->
           begin
             let challenger = Challenge.challenger challenge in
             let game_name = final_game_name challenge game_name in
             Logs.debug (fun m ->
                 m "successfully deleted challenge %d" id#get_id);

             let purge_challenger =
               ChallengesCriticalSection.purge_user_challenges
                 challenges.challenges
                 challenger
             and purge_challengee =
               ChallengesCriticalSection.purge_user_challenges
                 challenges.challenges
                 user
             in
             purge_challenger >>= (fun () -> purge_challengee)
             >|=
               (fun () ->
                 (* We shouldn't accept the challenge before the
                  game has been created. Maybe add a game creation module
                  as a parameter to this module, and call that function
                  before accepting the challenge *)
                 Challenge.accept challenge
               )
             >|=
               (fun () ->
                 Internal_types.Accept {
                     game_name;
                     id;
                     challenger;
                     chalengee = user;
                   }
               )
           end
        | Ttt_server_lib_types.Id_not_present ->
           begin
             Logs.debug
               (fun m ->
                 m "challenge with id %d not present" id#get_id);
             Lwt.return Internal_types.Declined
           end

    let accept challenges id user =
      accept' challenges id user

    let challenge_matches challenge game_name =
      match Challenge.game_name challenge, game_name with
      | Some g1, Some g2 -> g1 = g2
      | _ -> true

    let attempt_find_matching_challenge challenges user game_name =
      Logs.debug (fun m ->
          m "challenge api attempt find matching challenge");

      let rec aux = function
        | [] -> Lwt.return Internal_types.Declined
        | challenge :: challenge_tail ->
           let challenger = Challenge.challenger challenge
           and id = Challenge.id challenge in
           if challenger = user then
             (Logs.err (fun m ->
                  m "%s %s %s"
                    "Attempting to accept a challenge from the same player."
                    "challenge_db.public_challenges_for_user shouldn't"
                    "return challenges from that user");
              Lwt.return Internal_types.Declined)
           else if challenge_matches challenge game_name then
             begin
               Logs.debug (fun m -> m "Found matching challenge");
               let open Lwt in
               accept' challenges ?game_name id user >>=
                 function
             | Internal_types.Declined ->
                Logs.debug (fun m -> m "couldn't combine, too late");
                aux challenge_tail
             | accept ->
                Logs.debug (fun m -> m "challenges combined");
                Lwt.return accept
             end
           else
             aux challenge_tail
      in
      aux (ChallengesCriticalSection.public_challenges_for_user
                challenges.challenges
                user)


    let create_challenge challenges ?opponent:opponent user2 game_name =
      let id = Id_generator.next () in
      Logs.debug (fun m ->
          m "Challenge api create %d"
                 id#get_id);
      create challenges ?opponent user2 game_name id

    let new_challenge challenges challenger ?opponent:opponent game_name =
      match opponent with
      | None ->
         begin
           let open Lwt in
           attempt_find_matching_challenge
             challenges
             challenger
             game_name
           >|=
             function
             | Internal_types.Accept a ->
                Challenge_accepted a
             | Internal_types.Declined ->
                let challenge =
                  create_challenge
                    challenges
                    challenger
                    game_name
                in
                Ttt_server_lib_types.Challenge_created(
                    Challenge.id challenge, Challenge.event challenge
                  )
         end
      | Some opponent ->
         if opponent = challenger then
           Lwt.return
             (Ttt_server_lib_types.Error("You can't challenge yourself!"))
         else
           let challenge =
             create_challenge challenges ~opponent challenger game_name
           in
           Lwt.return (
               Ttt_server_lib_types.Challenge_created(
                   Challenge.id challenge, Challenge.event challenge
                 )
             )
end
