open Types

module GameInProgress : GAME_IN_PROGRESS =
  functor (Game : GAME) (Piece : PIECE) ->
  struct
  module Game = Game(Piece)
    type t =
      {
        game : Game.t;
        players : player -> string;
      }

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

    let user_status game user =
      match user_to_player game user with
      | Some p ->
         if p = Game.player_on game.game then
           `Play
         else
           `Wait
      | None -> `Watch

    let move game ~row ~column user =
(*      if user_status game user = `Play then
        let player = match user_to_player game user with
          |Some player -> player | None -> failwith "impossible" in
        let result = (Board.move game.board ~row ~column (piece_of player) :> move_result) in
        game.next_player <- next_player game.next_player;
        result
      else
        `WrongPlayer*)
      match user_to_player game user with
        None -> `WrongPlayer
      | Some player ->
         Game.move game.game ~row ~column player

    let piece_at game = Game.piece_at game.game

    let username_and_piece game player =
      let user = game.players player
      and piece = Game.piece_of player
      in user, piece
  end
