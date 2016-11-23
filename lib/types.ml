type player = P1 | P2

type result = [`KeepPlaying | `Won | `Lost | `Draw]

type move_result' =
  [
  | `InvalidMove
  | `OK
  ]

type move_result = [ move_result' | `WrongPlayer ]


module type PIECE =
  sig
    type t
    val pieces : t list
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
      move_result
    val piece_at : t -> row:int -> column:int -> Piece.t option
  end


module type GAME = functor (Piece : PIECE) ->
  sig
    type t
    val new_game : unit -> t
    val move : t -> row:int -> column:int -> player -> move_result
    val piece_at : t -> row:int -> column:int -> Piece.t option
    val player_on : t -> player
    val piece_of : player -> Piece.t
  end

type user_action = [
    `Play
  | `Wait
  | `Watch
  ]

type game_status =
  [
    `PlayOn
  | `Won of string
  | `Draw
  ]

module type GAME_IN_PROGRESS =
  functor (Game : GAME) (Piece : PIECE) ->
  sig
    type t
    val new_game : (player -> string) -> t
    val move : t -> row:int -> column:int -> string -> move_result
    val piece_at : t -> row:int -> column:int -> Piece.t option
    val username_and_piece : t -> player -> (string * Piece.t)
    val user_status : t -> string -> user_action
  end

type id = ID of int

module type REACT_DB = functor
    (Game : sig type t end) ->
  sig
    val put : id -> Game.t React.event * (?step:React.step -> Game.t -> unit) -> unit
    val delete : id -> unit
    val get_channel : id -> Game.t React.E.t (* Make this a "down" react already, since it's only for frontend use *)
    val get_update_function : id -> (?step:React.step -> Game.t -> unit)
  end

module type FB_REACT_GAME =
  functor (GameInProgress : GAME_IN_PROGRESS) (Game : GAME) (Piece : PIECE) (ReactDB : REACT_DB) ->
  sig
    type t
    val new_game : (player -> string) -> t * GameInProgress(Game)(Piece).t React.E.t
    val move : t -> row:int -> column:int -> string -> move_result
    val piece_at : t -> row:int -> column:int -> Piece.t option
    val username_and_piece : t -> player -> (string * Piece.t)
    val user_status : t -> string -> user_action
  end

module type GAMES =
  functor (FBGame : FB_REACT_GAME)
            (GameInProgress : GAME_IN_PROGRESS)
            (GameF : GAME) (Piece : PIECE) (ReactDB : REACT_DB) ->
  sig
    val new_game : string -> string -> id * GameInProgress(GameF)(Piece).t React.E.t
    val get_current_games : string -> (id * GameInProgress(GameF)(Piece).t React.E.t) list
    val get_react_game_by_id : id -> GameInProgress(GameF)(Piece).t React.E.t option
    val get_game_by_id : id -> GameInProgress(GameF)(Piece).t option
(*    val get_finished_games : string -> GameInProgress.t list
    val get_challenges_sent : string -> GameInProgress.t list
    val get_challenges_received : string -> GameInProgress.t list*)
  end

module type EXPORT =
  functor (GameInProgress : GAME_IN_PROGRESS)
            (GameF : GAME)
            (Piece : PIECE)
            (ReactDB : REACT_DB) ->
  sig
    type game
    val new_game : string -> string -> id * game
    val get_current_games : string -> (id * game) list
    val get_react_game_by_id : id -> game option
    val move : id -> row:int -> column:int -> string -> move_result
    val piece_at : game -> row:int -> column:int
                   -> Piece.t option React.event
    val username_and_piece : id -> player -> (string * Piece.t)
    val user_status : id -> string -> user_action
  end
