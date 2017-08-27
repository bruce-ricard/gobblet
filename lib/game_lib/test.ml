open Ttt_game_lib_games

let main () =
  let board = WBoard.empty_board () in
  let x : Ttt_game_lib_pieces.XOPiece.t = `X in
  match WBoard.move board ~row:1 ~column:1 x with
    `Ok _ -> ()
  | _ -> assert false

let () = main ()
