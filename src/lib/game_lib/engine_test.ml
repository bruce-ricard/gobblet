open Engine

module Game =
  struct
    type piece = unit
    type t = (bool * (bool * bool))
    type square = int
    type move = square
    type board = square -> piece option
    type move_result =
      [
      | `Ok of t
      | `Invalid
      ]
    let result _ = Won
    let all_moves _ = [1;2;3]
    let move (g : t) (m : move) (p : player) : move_result =
      match g,m with
      | (false, (b, c)), 1 -> `Ok (true, (b, c))
      | (a, (false, c)), 2 -> `Ok (a, (true, c))
      | (a, (b, false)), 3 -> `Ok (a, (b, true))
      | _ -> `Invalid

    let takeback g = ()
    let evaluate g _ p =
      match g with
      | (true, _) -> Win(0)
      | (false, (true, true)) -> Draw(0)
      | _ -> Unclear(0.)
  end

module DumbEngine = Engine.Make(Game)

let board_tester =
  let open Alcotest in
  pair bool (pair bool bool)

let test_play_all_moves game number_moves () =
  let all_moves = DumbEngine.play_all_moves game P1 in
  Alcotest.check
    Alcotest.int
    "all moves"
    number_moves
    (List.length all_moves)

let all_move_result_tester =
  let open Alcotest in
  slist (pair int board_tester) Pervasives.compare

let test_play_all_moves_builder game moves =
  let all_moves = DumbEngine.play_all_moves game P1 in
  Alcotest.check
    all_move_result_tester
    "all moves"
    moves
    all_moves

let test_play_all_moves1 () =
  let game = (false, (false, false))
  and expected_result = [
      (1, (true, (false, false)));
      (2, (false, (true, false)));
      (3, (false, (false, true)));
    ]
  in
  test_play_all_moves_builder game expected_result

let test_play_all_moves2 () =
  let game = (false, (false, true))
  and expected_result = [
      (1, (true, (false, true)));
      (2, (false, (true, true)));
    ]
  in
  test_play_all_moves_builder game expected_result

let test_play_all_moves3 () =
  let game = (true, (true, true))
  and expected_result = []
  in
  test_play_all_moves_builder game expected_result


let test_evaluate_position () =
  Alcotest.check
    Alcotest.int
    "test"
    0
    0

let engine_unit_suite = [
    "all moves 1", `Quick, test_play_all_moves (false, (false, false)) 3;
    "all moves 2", `Quick, test_play_all_moves (true, (false, false)) 2;
    "all moves 3", `Quick, test_play_all_moves (true, (true, true)) 0;
    "all moves 1", `Quick, test_play_all_moves1;
    "all moves 2", `Quick, test_play_all_moves2;
    "all moves 3", `Quick, test_play_all_moves3;
    "test", `Quick, test_evaluate_position;
  ]
