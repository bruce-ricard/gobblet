open Types

module FrontAndBackendReactGame : FB_REACT_GAME = functor
    (GameInProgress : GAME_IN_PROGRESS)
      (GameF : GAME)
      (Piece : PIECE)
      (ReactDB : REACT_DB) ->

  struct
    module GameM = GameInProgress(GameF)(Piece)
    module ReactDB = ReactDB(GameM)
    type t = GameM.t * int

    let new_id =
      let id = ref 0 in
      function () -> incr id; !id

    let new_game (players : player -> string) : t * GameInProgress(GameF)(Piece).t React.E.t =
      let game = GameM.new_game players in
      let rgame, update_game = React.E.create () in
      let id = (new_id ()) in
      ReactDB.put id (rgame, update_game);
      ((game, id), rgame)

    let move (game, id) ~row ~column user =
        match GameM.move game ~row ~column user with
        | `OK -> (ReactDB.get_update_function id) game; `OK
        | x ->  x

    let piece_at (game, id) = GameM.piece_at game
    let username_and_piece (game, id) = GameM.username_and_piece game
    let user_status (game, id) = GameM.user_status game
  end
