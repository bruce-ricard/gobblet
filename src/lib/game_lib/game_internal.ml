open Ttt_game_lib_types

module Make (Board : BOARD) : GAME_INTERNAL
  with type piece = Board.piece =
  struct
    type t = {
        board : Board.t;
        mutable game_status : game_status
      }
    type piece = Board.piece

    let new_game () =
      {
        board = Board.empty_board ();
        game_status = PlayerOn P1
      }

    let next_player = function
        P1 -> P2 | P2 -> P1

    let piece_of =
      match Board.pieces with
      | [x] -> (function _ -> x)
      | [x;y] -> (function P1 -> x | P2 -> y)
      | _ -> failwith "Invalid number of pieces"

    let switch_player_on game =
      match game.game_status with
        PlayerOn p -> game.game_status <- PlayerOn (next_player p)
      | _ -> failwith "impossible switch_player_on"

    let player_on game =
      match game.game_status with
      | PlayerOn p -> p
      | _ -> failwith "game over"

    let update_game_status game = function
      | `KeepPlaying -> switch_player_on game
      | `Won -> game.game_status <- GameOver (`Won (player_on game))
      | `Lost -> game.game_status <- GameOver (`Won (next_player (player_on game)))
      | `Draw -> game.game_status <- GameOver `Drawn

    let act game player action =
      match game.game_status with
      | PlayerOn playerOn ->
         if player = playerOn then
           begin
             let board_result =
               action ()
             in
             match board_result with
             | `Ok status -> (update_game_status game status; `Ok)
             | `Invalid _ as i -> (i :> move_result)
           end
         else
           `Invalid `NotYourTurn
      | GameOver _ -> `Invalid `GameWasOver

    let place game square player : move_result =
      act game player
          (fun () -> Board.place game.board square (piece_of player))

    let move game move player =
      act game player (fun () -> Board.move game.board move)

    let piece_at game = Board.piece_at game.board

    let game_status game =
      game.game_status

  end
