open Internal_types
open Ttt_game_lib_types
open Ttt_common_lib_types

module Make (Game : GAME_IN_PROGRESS) =
  struct
    type piece = Game.piece
    type game = Game.t fb_game

    let new_game players =
      let event, update = React.E.create () in
      {
        game = Game.new_game players;
        event;
        update
      }

    let place game ~row ~column user =
      let result = Game.place game.game {row; column} user in
      game.update game.game;
      result

    let move game move user = `Ok `KeepPlaying

    let username_and_piece game player =
      Game.username_and_piece game.game player

    let piece_at game ~row ~column =
      React.E.map (Game.piece_at ~row ~column) game.event

    let game_status game =
      React.E.map Game.game_status game.event

    let refresh_game game =
      game.update game.game
  end
