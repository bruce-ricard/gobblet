open Ttt_server_lib_types
open Ttt_game_lib_types
open Ttt_common_lib_types

module Make : GAMES =
  functor (GameInProgress : GAME_IN_PROGRESS)
            (Piece : PIECE)
            (RDB : REACT_DB) ->
  struct
    module Game = GameInProgress(Piece)
    module RDB = RDB(Game)

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
    List.map (fun (id, game) ->
        let game =
          match RDB.get_channel (ID id) with
            None -> failwith "impossible ID miss" (*TODO:make this safer*)
          | Some game -> game
        in
        ID id, game)
             !games

  let get_game_by_id (ID id) =
    try
      Some (List.assoc id !games)
    with
      Not_found -> None

  let new_game user1 user2 =
    (* TODO: check that both users are actually users *)
    let players = users_to_player_function user1 user2 in
    let game = Game.new_game players in
    let ID id = (next_id ()) in
    let react_game, update_function = React.E.create () in
    RDB.put (ID id) (react_game, update_function);
    games := (id, game) :: !games;
    (ID id, react_game)

  let move (id : id) ~row ~column user =
    let game =
      match get_game_by_id id with
        Some g -> g
      | None -> failwith "no game"
    and update_function =
      RDB.get_update_function id
    in
    let result = Game.move game ~row ~column user in
    update_function game;
    result

  let username_and_piece (ID id) =
    let game = List.assoc id !games in
    Game.username_and_piece game

  let piece_at (ID id) ~row ~column =
    match RDB.get_channel (ID id) with
      None -> failwith "no such ID" (*TODO : better fail*)
    | Some game -> React.E.map (Game.piece_at ~row ~column)  game

  let refresh_game (ID id) =
    let game = List.assoc id !games in
    let update_function = RDB.get_update_function (ID id) in
    update_function game

  let get_react_game_by_id id =
    RDB.get_channel id

  let game_status id =
    match RDB.get_channel id with
    | None -> failwith "no such ID" (*TODO : better fail*)
    | Some game -> React.E.map Game.game_status game

  let () =
    ignore (new_game "bruce" "bruce2")

  end
