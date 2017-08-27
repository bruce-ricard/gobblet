open Ttt_game_lib_types

module Make (Game : GAME_INTERNAL)
       : GAME_IN_PROGRESS with type piece = Game.piece =

  struct

    type t =
      {
        game : Game.t;
        players : player -> string;
      }
    type piece = Game.piece
    let new_game players =
      let game = Game.new_game () in
      {game ; players}

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

    let move game ~row ~column user =
      match user_to_player game user with
        None -> `Invalid `WrongPlayer
      | Some player ->
         Game.move game.game ~row ~column player

    let piece_at game = Game.piece_at game.game

    let username_and_piece game player =
      let user = game.players player
      and piece = Game.piece_of player
      in user, piece

    let store game = failwith "not implemented"

    let restore storage = failwith "not implemented"

  end
