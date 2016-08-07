open Ttt

module TestBoard = Board(IntPiece)
module XOBoard = Board(XOPiece)

let main () =
  let open XOBoard in
  let print_result = function
      KeepPlaying -> print_endline "keep playing!"
    | Won P1 -> print_endline "player 1 won!"
    | Won P2 -> print_endline "player 2 won!"
  in
  let print_tuple (result,board) =
    print board; print_result result; print_endline "##################"
  in
  let b0 = empty_board () in
  print b0;
  print_endline "###################";
  let r1 = move_unsafe b0 0 0 P1 in
  print_tuple r1;
  let r2 = move_unsafe (snd r1) 1 1 P2 in
  print_tuple r2;
  let r3 = move_unsafe (snd r2) 0 1 P1 in
  print_tuple r3;
  let r4 = move_unsafe (snd r3) 0 2 P1 in
  print_tuple r4

let () = main ()
