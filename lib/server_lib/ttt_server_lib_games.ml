open Ttt_server_lib_types
open Ttt_game_lib_types
open Ttt_common_lib_types

module Make
         (Game : GAME_IN_PROGRESS)
         (Challenge_db : CHALLENGE_DB)
         (Id_generator : GAME_ID_GENERATOR)
         (RDB : REACT_DB)
         (Challenge_RDB : CHALLENGE_REACT_DB)
         (Game_DB : GAME_DB with type game = Game.t)
         (Game_archive_db : GAME_ARCHIVE_DB with type game = Game.t)
         (User_DB : sig val exists : string -> bool end)
       : GAMES with type piece = Game.piece =

  struct
    module RDB = RDB(Game)

    type game = Game.t
    type piece = Game.piece

    let challenge_db = Challenge_db.load ()

    let () = Random.self_init ()

    let users_to_player_function user1 user2 = function
        P1 -> user1
      | P2 -> user2

    let get_current_games = Game_DB.get_games_for_user

    let challenge_event = Challenge_db.event_listener challenge_db

    let get_public_challenges user =
      let get_challenges () =
        List.map (fun (id, name) -> id#get_id, name) @@
          Challenge_db.public_challenges_for_user challenge_db user in
      {
        event = React.E.map get_challenges challenge_event;
        initial_data = get_challenges ()
      }

    let get_private_challenges user =
      let get_challenges () =
        List.map (fun (id, name) -> id#get_id, name) @@
          Challenge_db.private_challenges_for_user challenge_db user in
      {
        event = React.E.map get_challenges challenge_event;
        initial_data = get_challenges ()
      }

    let get_game_by_id = Game_DB.get_game

    let new_game ?random_side:(random_side=true) id user1 user2 =
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
        let game = Game.new_game players in
        let react_game, update_function = React.E.create () in
        RDB.put id (react_game, update_function);
        Game_DB.put_game id user1 user2 game;
        true

    let accept_challenge id user =
      match Challenge_db.remove challenge_db id with
      | Deleted(challenger) ->
         begin
           Logs.debug (fun m ->
               m "successfully deleted challenge %d" id#get_id);
           if new_game id challenger user then
             (Challenge_RDB.accept id;
              Logs.debug (fun m -> m "successfully created game %d" id#get_id);
              true)
           else
             (Logs.err (fun m ->
                  m "Attempted to accept a challenge from the same player");
               false)
         end
      | Id_not_present ->
         Logs.debug (fun m -> m "challenge with id %d not present" id#get_id);
         false

    let attempt_accept_challenge user =
      let rec aux () =
        match Challenge_db.public_challenges_for_user challenge_db user with
          [] -> None
        | (id,challenger) :: _ ->
           if challenger = user then
             (Logs.warn (fun m ->
                  m "%s %s %s"
                    "Attempting to accept a challenge from the same player."
                    "challenge_db.public_challenges_for_user shouldn't"
                    "return challenges from that user");
              None)
           else if accept_challenge id user then
             Some id
           else
             aux ()
      in
      aux ()

    let create_challenge ?opponent:opponent user2 =
      let id = Id_generator.next () in
      Challenge_db.create challenge_db ?opponent user2 id;
      id,(Challenge_RDB.create id)

    let new_challenge ?opponent:opponent challenger =
      if User_DB.exists challenger then
        match opponent with
        | None ->
           begin
             match attempt_accept_challenge challenger with
             | Some id -> Challenge_accepted id
             | None ->
                let id, event = create_challenge challenger in
                Challenge_created(id, event)

           end
        | Some opponent ->
           if opponent = challenger then
             Error("You can't challenge yourself!")
           else
             if User_DB.exists opponent then
               let id, event = create_challenge ~opponent challenger in
               Challenge_created(id, event)
             else
               Error(Printf.sprintf "\"%s\" is not a valid player." opponent)
      else
        begin
          Logs.err (fun m ->
              m "Impossible case, the current player has to exist");
          Error(Printf.sprintf "\"%s\" is not a valid player." challenger)
        end

    let archive_game id =
      match Game_DB.get_game id with
      | Some game ->
         begin
           Game_DB.delete_game id;
           RDB.delete id;
           Game_archive_db.put_game id game;
           Logs.debug (fun m -> m "archived game %d" id#get_id)
         end
      | None ->
         Logs.warn (fun m ->
             m "Attempted to archive game %d, which doesn't exist" id#get_id
           )

    let move (id : id) ~row ~column user =
      let game =
        match get_game_by_id id with
          Some g -> g
        | None -> failwith "no game"
      and update_function =
        RDB.get_update_function id
      in
      let result = Game.move game ~row ~column user in
      let () = match Game.game_status game with
        | `GameOver _ -> archive_game id
        | _ -> () in
      update_function game;
      result

    let username_and_piece id =
      match Game_DB.get_game id with
      | Some game -> Game.username_and_piece game
      | None -> failwith "invalid ID"

    let piece_at id ~row ~column =
      match RDB.get_channel id with
        None -> failwith "no such ID" (*TODO : better fail*)
      | Some game -> React.E.map (Game.piece_at ~row ~column)  game

    let refresh_game id =
      match Game_DB.get_game id with
      | Some game -> let update_function = RDB.get_update_function id in
                     update_function game
      | None -> failwith "invalid ID"

    let get_react_game_by_id id =
      RDB.get_channel id

    let game_status id =
      match RDB.get_channel id with
      | None -> failwith "no such ID" (*TODO : better fail*)
      | Some game -> React.E.map Game.game_status game

    let _ =
      Lwt.async (fun () ->
          let%lwt () = Lwt_unix.sleep 5. in
          Logs.debug (fun m -> m "KIKOO");
          let id = Id_generator.next() in
          Lwt.return (ignore (new_game id "bruce" "bruce2"))
        )

  end
