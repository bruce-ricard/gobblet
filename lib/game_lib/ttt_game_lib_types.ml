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

type game_storage_status =
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

module type BOARD = functor (Piece : PIECE) ->
  sig
    type t
    val empty_board : unit -> t
    val move :
      t ->
      row:int ->
      column:int ->
      Piece.t ->
      board_move_result
    (*    val board_status : t -> board_status*)
    val piece_at : t -> row:int -> column:int -> Piece.t option
    val serialize : t -> string
    val deserialize : string -> t option
  end


module type GAME = functor (Piece : PIECE) ->
  sig
    type t
    val new_game : unit -> t
    val move : t -> row:int -> column:int -> player -> move_result
    val piece_at : t -> row:int -> column:int -> Piece.t option
    (*    val player_on : t -> player*)
    val piece_of : player -> Piece.t
    val game_status : t -> game_status
  end

module type GAME_IN_PROGRESS =
  functor (Piece : PIECE) ->
  sig
    type t
    val new_game : (player -> string) -> t
    val move : t -> row:int -> column:int -> string -> move_result
    val piece_at : t -> row:int -> column:int -> Piece.t option
    val username_and_piece : t -> player -> (string * Piece.t)
    val game_status : t -> game_in_progress_status
    val store : t -> game_storage_status
    val restore : game_storage_status -> t option
  end
