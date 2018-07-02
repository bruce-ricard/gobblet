type result =
  Won | Lost | NotOver

module Make(Game : Ttt_game_lib_types.GAME_IN_PROGRESS) =
  struct

    let evaluate_move game player move =
      match Game.move game move player with
      | `Invalid _ -> `Invalid
      | `Ok -> ok

    let evaluate game player moves =
      let mvs = List.map (evaluate_move game player) moves
      in

      [ Won ]

    let rec best_move = function
      | [] -> None
      | [x] -> Some x
      | (move, eval) :: moves ->
         match best_move moves with
         | None -> Some (move, eval)
         | Some (move', eval') ->
            if (eval > eval') then
              Some (move, eval)
            else
              Some (move', eval')
  end
