open Ttt_server_lib_types
open Ttt_game_lib_types
open Ttt_common_lib_types

module type GAME =
  sig
    type game
    val new_game : (player -> string) -> id -> game
  end

module Make
         (Challenges : Ttt_server_lib_types.CHALLENGES)
         (Id_generator : GAME_ID_GENERATOR)
         (Game_archive_db : GAME_ARCHIVE_DB)
         (Game_DB : GAME_DB)
         (Tttc : GAME with type game = GameTypes.tttc)
         (Tttxo : GAME with type game = GameTypes.tttxo)
         (ThreeMenMorris : GAME with type game = GameTypes.three_men_morris)
         (User_DB : sig val exists : string -> bool end)
       : Ttt_server_lib_types.GAMES
  =
  struct
    module Games = Ttt_server_lib_game_list
    module Challenge = Ttt_server_lib_challenge

    let challenge_db = Challenges.load ()

    let () = Random.self_init ()
    let () = Challenges.send_updates challenge_db

    let users_to_player_function user1 user2 = function
        P1 -> user1
      | P2 -> user2

    let get_current_games = Game_DB.get_games_for_user

    let challenge_event = Challenges.event_listener challenge_db

    let challenge_to_frontend challenge =
      {
        id = (Challenge.id challenge)#get_id;
        challenger = Challenge.challenger challenge;
        game_type = Challenge.game_name challenge;
      }

    let get_public_challenges user =
      let get_challenges () =
        List.map challenge_to_frontend @@
          Challenges.public_challenges_for_user challenge_db user in
      React.E.map get_challenges challenge_event

    let get_private_challenges user =
      let get_challenges () =
        List.map challenge_to_frontend @@
          Challenges.private_challenges_for_user challenge_db user in
      React.E.map get_challenges challenge_event

    let get_game =
      Game_DB.get_game

    let random_game () =
      let random_int = Random.int 3 in
      match random_int with
        0 -> `TicTacToeClassical
      | 1 -> `TicTacToeXOnly
      | 2 -> `ThreeMenMorris
      | _ ->
         begin
           Logs.warn (fun m -> m "Invalid random_int in random_game");
           `TicTacToeClassical
         end

    let new_game_creator game_name players id =
      let game_name = match game_name with
        | None -> random_game ()
        | Some g -> g in
      let open Ttt_server_lib_game_list in
      match game_name with
      | `TicTacToeClassical ->
         `TicTacToeClassical (Tttc.new_game players id)
      | `TicTacToeXOnly ->
         `TicTacToeXOnly (Tttxo.new_game players id)
      | `ThreeMenMorris ->
         `ThreeMenMorris (ThreeMenMorris.new_game players id)

    let new_game
          ?random_side:(random_side=true)
          game_name
          id
          user1
          user2 =
      if user1 = user2 then
        false
      else
        let user1,user2 =
          if random_side then
            if Random.int 2 == 0 then
              (Logs.debug (fun m -> m "Users randomly NOT swapped");
               user1,user2)
            else
              (Logs.debug (fun m -> m "Users randomly swapped");
              user2,user1)
          else
            (Logs.debug (fun m -> m "Users kept in order");
             user1, user2)
        in
        let players = users_to_player_function user1 user2 in
        let game = new_game_creator game_name players id in
        Game_DB.put_game id user1 user2 game;
        true

    let final_game_name challenge game_name =
      match (Challenge.game_name challenge), game_name with
        None, None -> None
      | None, Some(g) | Some(g), None-> Some(g)
      | Some(g), Some(_) ->
         begin
           Logs.err (fun m -> m "Incompatible games in final_game_name");
           Some(g)
         end

    let accept_challenge_internal ?game_name id user =
      match Challenges.remove challenge_db id with
      | Deleted(challenge) ->
         begin
           let challenger = Challenge.challenger challenge in
           let game_name = final_game_name challenge game_name in
           Logs.debug (fun m ->
               m "successfully deleted challenge %d" id#get_id);
           if new_game game_name id challenger user then
             (Challenge.accept challenge;
              Logs.debug
                (fun m -> m "successfully created game %d" id#get_id);
              true)
           else
             begin
               Logs.err (fun m -> m "Error creating new game");
               false
             end
         end
      | Id_not_present ->
              Logs.debug
                (fun m -> m "challenge with id %d not present" id#get_id);
              false

    let accept_challenge = accept_challenge_internal ?game_name:None

    let challenge_matches challenge game_name =
      match Challenge.game_name challenge, game_name with
      | Some g1, Some g2 -> g1 = g2
      | _ -> true

    let attempt_accept_challenge user game_name =
      let rec aux () =
        match Challenges.public_challenges_for_user challenge_db user with
          [] -> None
        | challenge :: _ ->
           let challenger = Challenge.challenger challenge
           and id = Challenge.id challenge in
           if challenger = user then
             (Logs.warn (fun m ->
                  m "%s %s %s"
                    "Attempting to accept a challenge from the same player."
                    "challenge_db.public_challenges_for_user shouldn't"
                    "return challenges from that user");
              None)
           else if challenge_matches challenge game_name
                   && accept_challenge_internal id user then
             Some id
           else
             aux ()
      in
      aux ()

    let create_challenge ?opponent:opponent user2 game_name =
      let id = Id_generator.next () in
      Challenges.create challenge_db ?opponent user2 game_name id

    let new_challenge ?opponent:opponent challenger game_name =
      if User_DB.exists challenger then
        match opponent with
        | None ->
           begin
             match attempt_accept_challenge challenger game_name with
             | Some id -> Challenge_accepted id
             | None ->
                let challenge = create_challenge challenger game_name in
                Challenge_created(
                    Challenge.id challenge, Challenge.event challenge
                  )
           end
        | Some opponent ->
           if opponent = challenger then
             Error("You can't challenge yourself!")
           else
             if User_DB.exists opponent then
               let challenge = create_challenge ~opponent challenger game_name
               in
                Challenge_created(
                    Challenge.id challenge, Challenge.event challenge
                  )
             else
               Error(Printf.sprintf "\"%s\" is not a valid player." opponent)
      else
        begin
          Logs.err (fun m ->
              m "Impossible case, the current player has to exist");
          Error(Printf.sprintf "\"%s\" is not a valid player." challenger)
        end

    let game_api_to_game_db : GameTypes.named_game -> 'a =
      let open Ttt_server_lib_game_list in
      function
      | `TicTacToeClassical game ->
         `TicTacToeClassical game.game
      | `TicTacToeXOnly game ->
         `TicTacToeXOnly game.game
      | `ThreeMenMorris game ->
         `ThreeMenMorris game.game

    let archive_game id =
      match Game_DB.get_game id with
      | Some game_api ->
         begin
           Game_DB.delete_game id;
           Game_archive_db.put_game id game_api;
           Logs.debug (fun m -> m "archived game %d" id#get_id)
         end
      | None ->
         Logs.warn (fun m ->
             m "Attempted to archive game %d, which doesn't exist" id#get_id
           )

    let _ =
      Lwt.async (fun () ->
          Lwt.bind
            (Lwt_unix.sleep 5.)
            (fun () ->
              let id = Id_generator.next() in
              ignore (new_game
                 ~random_side:false
                 (Some `TicTacToeClassical)
                 id "bruce" "bruce2");
              let id = Id_generator.next() in
              ignore (new_game
                 ~random_side:false
                 (Some `TicTacToeClassical)
                 id "bruce" "bruce2");
              let id = Id_generator.next() in
              ignore (new_game
                        ~random_side:false
                        (Some `TicTacToeClassical)
                        id "bruce" "bruce2");
              let id = Id_generator.next() in
              ignore (new_game
                        ~random_side:false
                        (Some `ThreeMenMorris)
                        id "bruce" "bruce2");
              Lwt.return ()
            )
        )
  end
