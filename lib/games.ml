open Types

module MemoryGames : GAMES =
  functor  (FBGame : FB_REACT_GAME)
             (GameInProgress : GAME_IN_PROGRESS) (Game : GAME) (Piece : PIECE) (RDB : REACT_DB) ->
  struct
  module GameM = FBGame(GameInProgress)(Game)(Piece)(RDB)
  module RDB = RDB(GameM)

  let games = ref []

  let next_id =
    let id = ref 0 in
    function () ->
             incr id;
             ID !id

  let users_to_player_function user1 user2 = function
      P1 -> user1
    | P2 -> user2

  let get_current_games user =
    (*  List.filter (fun game ->*)
    List.map (fun (x) ->  x) !games

  let get_game_by_id id =
    try
      Some (List.assoc id !games)
    with
      Not_found -> None

  let new_game user1 user2 =
    (* TODO: check that both users are actually users *)
    let players = users_to_player_function user1 user2 in
    let (game, react) = GameM.new_game players in
    let id = (next_id ()) in
    RDB.put id react;
    games := (id, game) :: !games;
    (id, (game, react))

  let move id ~row ~column user =
    let game =
      match game_game_by_id id with
        Some g -> g
      | None -> failwith "no game"
    and update_function =
      RDB.get_update_function id
    in
    GameM.move game update_function ~row ~column user

  let () =
    ignore (new_game "bruce" "bruce2")

  end
