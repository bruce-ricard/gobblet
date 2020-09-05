

module NilReporter =
  struct
    type t = unit
    let get () = ()
    let report_game_end () _ _ = ()
  end

module TicTacToeClassical =
  Ttt_game_lib_games.TicTacToeClassical(NilReporter)

module TTT = TicTacToeClassical

let players =
  | P1 -> "p1"
  | P2 -> "p2"

let next_player = function
  | "p1" -> "p2"
  | "p2" -> "p1"
  | _ -> assert false

let new_game () = TTT.new_game players (new id 1)

let test_finished_game_returns_error () =
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
  let () = match TTT.move game {row = 0; column = 0} player_on with
    | `Ok `KeepPlaying -> ()
    | _ -> assert false
  in
  let player_on = next_player player_on in
  let () = match TTT.move game {row = 1; column = 0} player_on with
    | `Ok `KeepPlaying -> ()
    | _ -> assert false
  in
  let player_on = next_player player_on in
  let () = match TTT.move game {row = 0; column = 1} player_on with
    | `Ok `KeepPlaying -> ()
    | _ -> assert false
  in
  let player_on = next_player player_on in
  let () = match TTT.move game {row = 1; column = 1} player_on with
    | `Ok `KeepPlaying -> ()
    | _ -> assert false
  in
  let player_on = next_player player_on in
  let () = match TTT.move game {row = 0; column = 2} player_on with
    | `Ok `Won -> ()
    | _ -> assert false
