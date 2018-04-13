open Ttt_game_lib_types
open Ttt_common_lib_types

open Ttt_server_lib_types.FrontendBackendGame

module GameTypes = Ttt_server_lib_types.GameTypes

module type GAME =
  sig
    type game
    val new_game : (player -> string) -> id -> game
  end

module type GAMES_AND_ARCHIVING =
  sig
    include Ttt_server_lib_types.GAMES
    include Ttt_server_lib_types.ARCHIVE
  end

module Make
         (Challenges : Ttt_server_lib_types.CHALLENGE_API)
         (Game_archive_db : Ttt_server_lib_types.GAME_ARCHIVE_DB)
         (Game_DB : Ttt_server_lib_types.GAME_DB)
         (Tttc : GAME with type game = GameTypes.tttc)
         (Tttxo : GAME with type game = GameTypes.tttxo)
         (ThreeMenMorris : GAME with type game = GameTypes.three_men_morris)
         (User_DB : sig val exists : string -> bool end)
       : GAMES_AND_ARCHIVING
  =
  struct
    module Games = Ttt_server_lib_game_list

    let challenge_db = Challenges.load ()

    let () = Random.self_init ()

    let users_to_player_function user1 user2 = function
        P1 -> user1
      | P2 -> user2

    let get_current_games = Game_DB.get_games_for_user

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
           Logs.err (fun m -> m "Invalid random_int in random_game");
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
        begin
          Logs.debug (fun m -> m "Creating new game");
          let user1,user2 =
            if random_side then
              if Random.int 2 == 0 then
                user1,user2
              else
                user2,user1
            else
              user1, user2
          in
          let players = users_to_player_function user1 user2 in
          let game = new_game_creator game_name players id in
          Game_DB.put_game id user1 user2 game;
          true
        end

    let new_challenge ?opponent user game_name =
      Logs.debug (fun m -> m "games new challenge");
      let report_error user =
        Logs.err (fun m ->
            m "%s %s %s"
              "Attempting to create challenge with user "
              "who doesn't exist: "
              user
          );
        Lwt.return (Ttt_server_lib_types.Error(
            "This challenge contains illegal users"))
      in
      let unknown_opponent =
        match opponent with
        | Some o -> if User_DB.exists o then None else Some o
        | None -> None
      in

      if User_DB.exists user then
        match unknown_opponent with
        | None ->
           let open Lwt in
           Challenges.new_challenge
             challenge_db
             user
             ?opponent
             game_name >|=
             let open Ttt_server_lib_types in
             (function
              | Challenge_accepted accepted ->
                 (new_game
                    accepted.game_name
                    accepted.id
                    accepted.challenger
                    accepted.chalengee;
                  Challenge_accepted accepted)
              | x -> x)

        | Some unknown ->
           report_error unknown
      else
        report_error user

    let accept_challenge id user =
      let open Ttt_server_lib_types in
      let open Lwt in
      Logs.debug (fun m -> m "games accept challenge");
      Challenges.accept challenge_db id user >|=
        function
      | Accept accepted ->
         Logs.debug
           (fun m ->
             m "successfully created game %d" id#get_id
           );

         new_game
           accepted.game_name
           accepted.id
           accepted.challenger
           accepted.chalengee
      | Declined -> false

    let get_public_challenges =
      Challenges.public_challenges_for_user challenge_db

    let get_private_challenges =
      Challenges.private_challenges_for_user challenge_db

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
         Logs.err (fun m ->
             m "Attempted to archive game %d, which doesn't exist" id#get_id
           )

    let _ =
      Lwt.async (fun () ->
          Lwt.bind
            (Lwt_unix.sleep 5.)
            (fun () ->
              let id = new id 1000 in
              ignore (new_game
                 ~random_side:false
                 (Some `TicTacToeClassical)
                 id "bruce" "bruce2");
              let id = new id 1001 in
              ignore (new_game
                 ~random_side:false
                 (Some `TicTacToeClassical)
                 id "bruce" "bruce2");
              let id = new id 1002 in
              ignore (new_game
                        ~random_side:false
                        (Some `TicTacToeClassical)
                        id "bruce" "bruce2");
              let id = new id 1003 in
              ignore (new_game
                        ~random_side:false
                        (Some `ThreeMenMorris)
                        id "bruce" "bruce2");
              Lwt.return ()
            )
        )
  end
