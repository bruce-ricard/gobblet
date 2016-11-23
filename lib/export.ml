open Types

module Make : EXPORT =
  functor (GameInProgress : GAME_IN_PROGRESS)
            (GameF : GAME)
            (Piece : PIECE)
            (ReactDB : REACT_DB)->
  struct
    module GameM = GameInProgress(GameF)(Piece)
    module ReactDB = ReactDB(GameM)
    module Piece = Piece

    type game = GameM.t React.E.t

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
    failwith "not implemented yet"

  let get_game_by_id id =
    try
      Some (List.assoc id !games)
    with
      Not_found -> None

  let get_react_game_by_id id =
    Some (ReactDB.get_channel id)

  let new_game user1 user2 =
    (* TODO: check that both users are actually users *)
    let players = users_to_player_function user1 user2 in
    let game = GameM.new_game players in
    let react, update_function = React.E.create () in
    let id = (next_id ()) in
    ReactDB.put id (react, update_function);
    games := (id, game) :: !games;
    (id, react)

  let move id ~row ~column user =
    let game =
      match get_game_by_id id with
        Some g -> g
      | None -> failwith "no game"
    and update_function =
      ReactDB.get_update_function id
    in
    let result = GameM.move game ~row ~column user in
    update_function game;
    result

  let () =
    ignore (new_game "bruce" "bruce2")

  let piece_at game ~row ~column =
(*    let game = match get_game_by_id id with
        None -> failwith "no game with such id"
      | Some g -> g in*)
    React.E.map (fun g -> GameM.piece_at g ~row ~column) game

  let username_and_piece id =
    match get_game_by_id id with
      None -> failwith "no such game"
    | Some game -> GameM.username_and_piece game

  let user_status  id =
    match get_game_by_id id with
      None -> failwith "no such game"
    | Some game -> GameM.user_status game

  end
