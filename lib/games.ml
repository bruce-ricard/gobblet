open Types

module MemoryGames : GAMES =
  functor  (FBGame : FB_REACT_GAME)
             (GameInProgress : GAME_IN_PROGRESS) (Game : GAME) (Piece : PIECE) (RDB : REACT_DB) ->
  struct
  module GameM = FBGame(GameInProgress)(Game)(Piece)(RDB)

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
    let game, react_game = GameM.new_game players in
    let id = next_id () in
    let game_with_id = id, react_game in
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
    ignore (new_game "bruce" "bruce2")

  end
