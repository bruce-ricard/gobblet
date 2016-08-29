type player = P1 | P2
type move_result = [ Ttt.move_result | `WrongPlayer ]

module type GAME =
  sig
    type t
    val new_game : unit -> t
    val move : t -> row:int -> column:int -> player -> move_result
  end


module Game (Board : Ttt.BOARD) (Piece : Ttt.PIECE) : GAME =
  struct
    module Board = Board(Piece)
    type t = {
        board : Board.t;
        next_player : player
      }

    let new_game () =
      {
        board = Board.empty_board ();
        next_player = P1
      }

    let next_player = function
        P1 -> P2 | P2 -> P1

    let piece_of =
      match Piece.pieces with
      | [x] -> (function _ -> x)
      | [x;y] -> (function P1 -> x | P2 -> y)
      | _ -> failwith "Invalid number of pieces"

    let move game ~row ~column player =
      if player = game.next_player then
        (Board.move game.board ~row ~column (piece_of player) :> move_result)
      else
        `WrongPlayer

  end
