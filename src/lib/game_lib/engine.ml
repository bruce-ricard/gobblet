type result =
  Won | Lost | Drawn | NotOver

                         (*
type square = {
    row: int;
    column: int;
  }
                          *)
type player = P1 | P2
type evaluation =
  Win of int | Lose of int | Draw of int | Unclear of float

module type GAME =
  sig
    type t
    type piece
    type move
    type square
    type board = square -> piece option
    type move_result =
      [
      | `Ok of t
      | `Invalid
      ]

    val result: t -> result
    val all_moves : t -> move list
    val move: t -> move -> player -> move_result
    val takeback: t -> unit
    val evaluate: t -> (board -> evaluation) -> player -> evaluation
  end

module Make(Game : GAME) =
  struct

    (*
    let all_moves =
      [
        {row = 0; column = 0};
        {row = 0; column = 1};
        {row = 0; column = 2};
        {row = 1; column = 0};
        {row = 1; column = 1};
        {row = 1; column = 2};
        {row = 2; column = 0};
        {row = 2; column = 1};
        {row = 2; column = 2};
      ]
     *)

    let play_all_moves game player =
      let evals =
        List.map
          (fun m -> m,Game.move game m player)
          (Game.all_moves game)
      in
      List.fold_left
        (fun positions (square, result) ->
          match result with
          | `Ok g -> (square, g) :: positions
          | `Invalid -> positions
        )
        []
        evals

    let evaluate_position game =
      let result = Game.result game in
      let eval =
        match result with
        | Won -> Win(0)
        | Lost -> Lose(0)
        | Drawn -> Draw(0)
        | NotOver -> Unclear(0.)
      in
      Game.evaluate game (fun _ -> eval)

    let evaluate_move game player move =
      match Game.move game move player with
      | `Invalid  -> `Invalid
      | `Ok game -> `Eval (evaluate_position game)

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
