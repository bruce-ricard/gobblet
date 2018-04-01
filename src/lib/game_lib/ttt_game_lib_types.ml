type player = P1 | P2

type board_status = [`KeepPlaying | `Won | `Lost | `Draw]

type board_move_result =
  [
  | `Invalid of [`InvalidMove | `GameWasOver ]
  | `Ok of board_status
  ]

type invalid_move =
  [ `NotYourTurn | `InvalidMove | `GameWasOver | `WrongPlayer ]

type move_result =
  [
  | `Invalid of invalid_move
  | `Ok
  ]

type serialized_game =
  {
    player1 : string;
    player2 : string;
    board_status : string;
  }

module type PIECE =
  sig
    type t
    val pieces : t list
    val serialize : t -> char
    val deserialize : char -> t option
  end

type user_action = [
    `Play
  | `Wait
  | `Watch
  ]

type 'a game_result =
  [
  | `Won of 'a
  | `Drawn
  ]

type game_status =
  | GameOver of player game_result
  | PlayerOn of player

type game_in_progress_status = [
    `PlayOn of (string -> user_action)
  | `GameOver of string game_result
  ]

module type WINNER_WINS =
  sig
    val wins : bool
  end

type square = {
    row: int;
    column: int;
  }

type move = {
    origin: square;
    destination: square;
  }

module type BOARD =
  sig
    type t
    type piece

    val empty_board : unit -> t
    val pieces : piece list
    val place :
      t ->
      square ->
      piece ->
      board_move_result

    val move :
      t ->
      move ->
      board_move_result
    val piece_at : t -> row:int -> column:int -> piece option
    val serialize : t -> string
    val deserialize : string -> t option
  end

type draw_report = {
    player1 : string;
    player2 : string;
  }

type decisive_report = {
    winner : string;
    loser : string;
  }

type report_result =
  | Draw of draw_report
  | Decisive of decisive_report

module type REPORTER =
  sig
    type t
    val get : unit -> t
    val report_game_end :
      t -> report_result -> Ttt_common_lib_types.id -> unit
  end

module type GAME_INTERNAL =
  sig
    type t
    type piece
    val new_game : unit -> t
    val place : t -> square -> player -> move_result
    val move : t -> move -> player -> move_result
    val piece_at : t -> row:int -> column:int -> piece option
    (*    val player_on : t -> player*)
    val piece_of : player -> piece
    val game_status : t -> game_status
  end

module type GAME_IN_PROGRESS =
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
