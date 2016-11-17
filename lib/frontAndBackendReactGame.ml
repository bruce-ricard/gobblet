open Types

module FrontAndBackendReactGame : FB_REACT_GAME = functor
    (GameInProgress : GAME_IN_PROGRESS)
      (GameF : GAME)
      (Piece : PIECE)
      (ReactDB : REACT_DB)
      (Games : GAMES) ->

  struct
    module GameM = GameInProgress(GameF)(Piece)
    module ReactDB = ReactDB(GameM)

    type t = int

    let new_game (players : player -> string) =
      let game = GameM.new_game players in
      let react = React.E.create () in
      game, react

    let move = Games.move

    let move game update_function ~row ~column user =
        match GameM.move game ~row ~column user with
        | `OK -> update_function game; `OK
        | x ->  x

    let piece_at (game, id) = GameM.piece_at game
    let username_and_piece (game, id) = GameM.username_and_piece game
    let user_status (game, id) = GameM.user_status game
  end
