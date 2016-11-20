open Types

module FrontAndBackendReactGame : FB_REACT_GAME = functor
    (GameInProgress : GAME_IN_PROGRESS)
      (GameF : GAME)
      (Piece : PIECE)
      (ReactDB : REACT_DB)
  ->

  struct
    module GameM = GameInProgress(GameF)(Piece)
    module ReactDB = ReactDB(GameM)

    type t = int

    let new_id =
      let id = ref 0 in
      function () -> incr id; !id

    let new_game (players : player -> string) =
      let game = GameM.new_game players in
      let react_game, update_function = React.E.create () in
      let id = new_id () in
      ReactDB.put id (react_game, update_function);
      id, react_game

    let move = GameM.move

    let move id ~row ~column user =
      let update_function = ReactDB.get_update_function id in
        match GameM.move game ~row ~column user with
        | `OK -> update_function game; `OK
        | x ->  x

    let piece_at (game, id) = GameM.piece_at game
    let username_and_piece (game, id) = GameM.username_and_piece game
    let user_status (game, id) = GameM.user_status game
  end
