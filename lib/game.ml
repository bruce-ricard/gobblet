module Game (Board : Ttt.BOARD) (Piece : Ttt.PIECE) =
  struct
    module Board = Board(Piece)
    type t = {
        board : Board.t;
        next_player : Ttt.player
      }

    type move_result = [ Ttt.move_result | `WrongPlayer ]

    let new_game () =
      {
        board = Board.empty_board ();
        next_player = Ttt.P1
      }

    let next_player = let open Ttt in function
        P1 -> P2 | P2 -> P1

    let move game ~row ~column player =
      if player = game.next_player then
        (Board.move game.board ~row ~column (Piece.piece_of player) :> move_result)
      else
        `WrongPlayer

  end
