open Ttt_common_lib_types
open Ttt_game_lib_types

module Make (Game : GAME_INTERNAL)
  =
  struct
    module Types =
      struct
        type t =
          {
            game : Game.t;
            players : player -> string;
            id : id;
          }
        type piece = Game.piece
      end

    include Types

    module Make (Reporter : REPORTER)
           : (GAME_IN_PROGRESS
              with type piece = Game.piece
              with type t = Types.t) =
      struct
        include Types

        let new_game players id =
          let game = Game.new_game () in
          {game ; players; id}

        let player_to_user game = game.players

        let user_to_player game user =
          if game.players P1 = user then
            Some P1
          else if game.players P2 = user then
            Some P2
          else
            None

        let user_status game player_on user =
          match user_to_player game user with
          | Some p ->
             if p = player_on then
               `Play
             else
               `Wait
          | None -> `Watch

        let game_status game =
          match Game.game_status game.game with
          | GameOver (`Won p) -> `GameOver (`Won (player_to_user game p))
          | GameOver `Drawn -> `GameOver `Drawn
          | PlayerOn p -> `PlayOn (user_status game p)

        let other_player = function
            P1 -> P2 | P2 -> P1

        let report_if_game_over game =
          match Game.game_status game.game with
          | GameOver (`Won p) ->
             let winner = game.players p
             and loser = game.players (other_player p) in
             Reporter.report_game_end
               (Reporter.get ())
               (Decisive {winner; loser})
               game.id
          | GameOver(`Drawn) ->
             let player1 = game.players P1
             and player2 = game.players P2 in
             Reporter.report_game_end
               (Reporter.get ())
               (Draw {player1; player2})
               game.id
          | _ -> ()

        let act game user action =
          match user_to_player game user with
          | None -> `Invalid `WrongPlayer
          | Some player ->
             let result = action player in
             (match result with
              | `Ok -> report_if_game_over game
              | `Invalid _ ->
                 Logs.debug (fun m -> m "Not reporting, invalid move")
             );
             result

        let place game square user =
          act game user
              (Game.place game.game square)

        let move game move user =
          act game user
              (Game.move game.game move)

        let piece_at game = Game.piece_at game.game

        let username_and_piece game player =
          let user = game.players player
          and piece = Game.piece_of player
          in user, piece

        let store game = failwith "not implemented"

        let restore storage = failwith "not implemented"
      end
  end
