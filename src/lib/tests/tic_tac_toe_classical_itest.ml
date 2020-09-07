open Ttt_game_lib_types

class id = Ttt_common_lib_types.id

module type TIC_TAC_TOE_CLASSICAL =
  sig
    type t
    type piece
    val new_game : (player -> string) -> Ttt_common_lib_types.id -> t
    val place : t -> square -> string -> move_result
    val move : t -> move -> string -> move_result
    val piece_at : t -> row:int -> column:int -> piece option
    val username_and_piece : t -> player -> (string * piece)
    val game_status : t -> game_in_progress_status
    val store : t -> serialized_game
    val restore : serialized_game -> t option
  end

module NilReporter =
  struct
    type t = unit
    let get () = ()
    let report_game_end () _ _ = ()
  end

module TicTacToeClassical =
  Ttt_game_lib_games.TicTacToeClassical(NilReporter)

module TTT = TicTacToeClassical

let players = function
  | P1 -> "p1"
  | P2 -> "p2"

let next_player = function
  | "p1" -> "p2"
  | "p2" -> "p1"
  | _ -> assert false

let new_game () = TTT.new_game players (new id 1)

let move ~row ~column =
  {
    origin = {row; column;};
    destination = {row = -1; column = -1}
  }

let poly_to_string = function
  | `Draw -> "Draw"
  | `Drawn -> "Drawn"
  | `GameOver -> "GameOver"
  | `GameWasOver -> "GameWasOver"
  | `Invalid -> "Invalid"
  | `InvalidMove -> "InvalidMove"
  | `KeepPlaying -> "KeepPlaying"
  | `Lost -> "Lost"
  | `NotYourTurn -> "NotYourTurn"
  | `O -> "O"
  | `Ok -> "Ok"
  | `Password -> "Password"
  | `Play -> "Play"
  | `PlayOn -> "PlayOn"
  | `Quick -> "Quick"
  | `Submit -> "Submit"
  | `Text -> "Text"
  | `ThreeMenMorris -> "ThreeMenMorris"
  | `TicTacToeClassical -> "TicTacToeClassical"
  | `TicTacToeXOnly -> "TicTacToeXOnly"
  | `Wait -> "Wait"
  | `Watch -> "Watch"
  | `Won -> "Won"
  | `WrongPlayer -> "WrongPlayer"
  | `X -> "X"

let move_result_to_string : move_result -> string = function
  | `Ok -> poly_to_string `Ok
  | `Invalid x ->
     Printf.sprintf "%s(%s)"
                    (poly_to_string `Invalid)
                    (poly_to_string x)


let board_move_result =
  Alcotest.testable
    (Fmt.of_to_string move_result_to_string)
    (=)

let check_move_result = Alcotest.check board_move_result
let check_move_ok = check_move_result "OK"

let test_player_on_can_move_1 () =
  let game = new_game () in
  let status = TTT.game_status game in
  let user_action =
    match status with
      | `GameOver _ -> assert false
      | `PlayOn f -> f
  in
  let player_on =
    match user_action "p1" with
    | `Watch -> assert false
    | `Play -> "p1"
    | `Wait -> "p2"
  in
  let () =
    check_move_ok
      `Ok
      (TTT.move game (move ~row:0 ~column:0) player_on)
  in ()

let test_player_on_can_move_2 () =
  let game = new_game () in
  let status = TTT.game_status game in
  let user_action =
    match status with
      | `GameOver _ -> assert false
      | `PlayOn f -> f
  in
  let player_on =
    match user_action "p1" with
    | `Watch -> assert false
    | `Play -> "p1"
    | `Wait -> "p2"
  in
  let () =
    check_move_ok
      `Ok
      (TTT.move game (move ~row:0 ~column:0) player_on)
  in
  let () =
    check_move_ok
      `Ok
      (TTT.move game (move ~row:0 ~column:1) (next_player player_on))
  in
  let () =
    check_move_ok
      `Ok
      (TTT.move game (move ~row:0 ~column:2) player_on)
  in ()

let test_returns_winner () =
  let game = new_game () in
  let status = TTT.game_status game in
  let user_action =
    match status with
      | `GameOver _ -> assert false
      | `PlayOn f -> f
  in
  let player_on =
    match user_action "p1" with
    | `Watch -> assert false
    | `Play -> "p1"
    | `Wait -> "p2"
  in
  let () = match TTT.move game (move ~row:0 ~column:0) player_on with
    | `Ok -> ()
    | _ -> assert false
  in
  let player_on = next_player player_on in
  let () = match TTT.move game (move ~row:1 ~column:0) player_on with
    | `Ok -> ()
    | _ -> assert false
  in
  let player_on = next_player player_on in
  let () = match TTT.move game (move ~row:0 ~column:1) player_on with
    | `Ok -> ()
    | _ -> assert false
  in
  let player_on = next_player player_on in
  let () = match TTT.move game (move ~row:1 ~column:1) player_on with
    | `Ok -> ()
    | _ -> assert false
  in
  let player_on = next_player player_on in
  let () = match TTT.move game (move ~row:0 ~column:2) player_on with
    | `Ok -> ()
    | _ -> assert false
  in
  match TTT.game_status game with
  | `GameOver (`Won p) when  p = player_on -> ()
  | _ -> assert false

let suite = [
    "player on can move 1", `Quick, test_player_on_can_move_1;
    "player on can move 2", `Quick, test_player_on_can_move_2;
    "returns winner", `Quick, test_returns_winner;
  ]
