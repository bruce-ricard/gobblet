open Types

module Make = functor (Board : BOARD) (Piece : PIECE) ->
  struct
    module Board = Board(Piece)
    type t = {
        board : Board.t;
        mutable next_player : player
      }

    let new_game () =
      {
        board = Board.empty_board ();
        next_player = P1
      }

    let next_player = function
        P1 -> P2 | P2 -> P1

    let player_on game = game.next_player

    let piece_of =
      match Piece.pieces with
      | [x] -> (function _ -> x)
      | [x;y] -> (function P1 -> x | P2 -> y)
      | _ -> failwith "Invalid number of pieces"

    let move game ~row ~column player =
      if player = game.next_player then
        begin
          let result =
            (Board.move game.board ~row ~column (piece_of player) :> move_result)
          in
          game.next_player <- next_player game.next_player;
          result
        end
      else
        `Invalid `WrongPlayer

    let piece_at game = Board.piece_at game.board

  end
