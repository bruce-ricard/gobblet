open GameInProgress
open Game

type id = ID of int

module type GAMES =
  functor (GameInProgress : GAME_IN_PROGRESS) (GameF : GAME) (Piece : Ttt.PIECE) ->
  sig
    val new_game : string -> string -> id * GameInProgress(GameF)(Piece).t
    val get_current_games : string -> (id * GameInProgress(GameF)(Piece).t) list
    val get_game_by_id : id -> GameInProgress(GameF)(Piece).t option
(*    val get_finished_games : string -> GameInProgress.t list
    val get_challenges_sent : string -> GameInProgress.t list
    val get_challenges_received : string -> GameInProgress.t list*)
  end

module MemoryGames : GAMES =
  functor (GameInProgress : GAME_IN_PROGRESS) (Game : GAME) (Piece : Ttt.PIECE) ->
  struct
  module GameInProgress = GameInProgress(Game)(Piece)

  let games = ref []

  let next_id =
    let id = ref 0 in
    function () ->
             incr id;
             ID !id

  let users_to_player_function user1 user2 = function
      P1 -> user1
    | P2 -> user2

  let new_game user1 user2 =
    let players = users_to_player_function user1 user2 in
    let game = GameInProgress.new_game players in
    let id = next_id () in
    let game_with_id = id, game in
    games := game_with_id :: !games;
    game_with_id

  let get_current_games user =
    (*  List.filter (fun game ->*)
    List.map (fun (id,x) -> id,x) !games

  let get_game_by_id id =
    try
      Some (List.assoc id !games)
    with
      Not_found -> None

  let () =
    ignore (new_game "bruce" "bruce1")

  end
