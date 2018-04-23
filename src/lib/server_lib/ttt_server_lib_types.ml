open Ttt_common_lib_types
open Ttt_game_lib_types

include Base_types

module type GAME_ID_GENERATOR =
  sig
    val next : unit -> id
  end

module type GAMES =
  sig
    val new_challenge : ?opponent:string -> string -> game_name option
                        -> id create_challenge_result Lwt.t
    val accept_challenge : id -> string -> bool Lwt.t

    val get_current_games : string -> (id * string) list
    val get_private_challenges : string -> frontend_challenge list React.event
    val get_public_challenges : string -> frontend_challenge list React.event

    val get_game : id -> GameTypes.named_game option
  end

module type GAME_API =
  sig
    type piece
    type game
    val new_game :
      (Ttt_game_lib_types.player -> string) ->
      Ttt_common_lib_types.id -> game
    val place :
      game ->
      row:int -> column:int -> string -> Ttt_game_lib_types.move_result
    val move :
      game ->
      Ttt_game_lib_types.move -> string -> Ttt_game_lib_types.move_result
    val username_and_piece :
      game ->
      Ttt_game_lib_types.player -> string * piece
    val piece_at :
      game ->
      row:int -> column:int -> piece option React.event
    val game_status :
      game ->
      Ttt_game_lib_types.game_in_progress_status React.event
    val refresh_game : game -> unit
  end

module type GAME_LIST =
  sig
    open GameTypes
    module TicTacToeClassical
           : GAME_API
           with type game = tttc
           with type piece = Ttt_game_lib_pieces.XOPiece.t

    module TicTacToeXOnly
           : GAME_API
           with type game = tttxo
           with type piece = Ttt_game_lib_pieces.XPiece.t

    module ThreeMenMorris
           : GAME_API
           with type game = three_men_morris
           with type piece = Ttt_game_lib_pieces.XOPiece.t
  end
