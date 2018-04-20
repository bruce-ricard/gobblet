open Ttt_game_lib_types

type semi_move_number = int

module type MOVE_DECIDER =
  sig
    val is_move_allowed: semi_move_number -> bool
    val is_place_allowed: semi_move_number -> bool
  end

module Make
         (W : WINNER_WINS)
         (Piece : PIECE)
         (MoveDecider : MOVE_DECIDER)
       : BOARD with type piece = Piece.t
  =
  struct
    type piece = Piece.t

    type t = Piece.t option array array * semi_move_number ref
    let pieces = Piece.pieces
    let empty_board () : t = Array.make_matrix 3 3 None, (ref 0)

    let piece_at_internal board ~row ~column =
      board.(row).(column)

    let piece_at (board, _) = piece_at_internal board

    let square_is_occupied board {row; column} =
      match piece_at_internal board ~row ~column with
      | Some _ -> true
      | None -> false

    let square_is_in_board {row; column} =
      if row >= 0 && row <= 2
         && column >= 0 && column <= 2 then
        true
      else
        begin
          Logs.err (fun m -> m "Square is not on the board!");
          false
        end

    let square_is_free board square =
      not @@ square_is_occupied board square

    let valid_placement (board, semi_move) ~row ~column =
      square_is_in_board {row; column}
      && square_is_free board {row; column}
      && MoveDecider.is_place_allowed !semi_move

    let squares_are_linked
          ({row = x1; column = y1} as s1)
          ({row = x2; column = y2} as s2)
      =
      let is_center row column =
        row = 1 && column = 1 in
      s1 <> s2 && (
        (x1 = x2 && (y2 = y1 + 1 || y2 = y1 - 1))
        || (y1 = y2 && (x2 = x1 + 1 || x2 = x1 -1))
        || (((is_center x1 y1) || (is_center x2 y2)) &&
              ((x2 = x1 + 1 && y2 = y1 + 1)
               || (x2 = x1 + 1 && y2 = y1 - 1)
               || (x2 = x1 - 1 && y2 = y1 + 1)
               || (x2 = x1 - 1 && y2 = y1 - 1))
           )
      )

    let valid_move
          (board, semi_move)
          {origin; destination}
      =
      square_is_in_board origin
      && square_is_in_board destination
      && MoveDecider.is_move_allowed !semi_move
      && (square_is_occupied board origin)
      && (square_is_free board destination)
      && (squares_are_linked origin destination)

    let lines board =
      let column n =
        [| board.(0).(n);  board.(1).(n);  board.(2).(n) |]
      and row n =
        board.(n)
      and diagonal d =
        let y x = if d then x else 2 - x in
        [| board.(0).(y 0);  board.(1).(y 1);  board.(2).(y 2) |]
      in
      [
        row 0;
        row 1;
        row 2;
        column 0;
        column 1;
        column 2;
        diagonal true;
        diagonal false
      ]

    let all_same (array : Piece.t option array ): Piece.t option =
      match array.(0), array.(1), array.(2) with
      | Some(x), Some(y), Some(z) when x = y && y = z -> Some(x)
                                                   | _ -> None

    let check_board board : Piece.t option =
      List.fold_left
        (fun result element ->
          match result, element with
            None, None -> None
          | Some(x), None | None, Some(x) -> Some(x)
          | Some(x), Some(y) -> if x = y then Some x else failwith "impossible check_board"
        )
        None
        (List.map all_same (lines board))

    let board_is_full board =
      let result = ref true in
      for x = 0 to 2 do
        for y = 0 to 2 do
          match board.(x).(y) with
            None -> result := false
          | Some(_) -> ()
        done
      done;
      !result

    let copy_board b =
      let result = Array.make 3 [||] in
      result.(0) <- Array.copy b.(0);
      result.(1) <- Array.copy b.(1);
      result.(2) <- Array.copy b.(2);
      result

    let board_status (board, _) =
      match check_board board with
      | None ->
         if board_is_full board then
          `Draw
         else
           `KeepPlaying
      | Some piece ->  if W.wins then `Won else `Lost

    let actually_place (board, semi_move) ~row ~column piece  =
      if valid_placement (board, semi_move) ~row ~column then
        begin
          board.(row).(column) <- Some piece;
          incr semi_move;
          `Ok (board_status (board, semi_move))
        end
      else
        `Invalid `InvalidMove

    let place (board : t) {row; column} piece : board_move_result =
      match board_status board with
      | `KeepPlaying -> actually_place board ~row ~column piece
      | _ -> `Invalid `GameWasOver

    let actually_move board move =
      if valid_move board move then
        begin
          let board = fst board
          and semi_move = snd board in
          let {origin; destination} = move in
          let piece = board.(origin.row).(origin.column) in
          board.(origin.row).(origin.column) <- None;
          board.(destination.row).(destination.column) <- piece;
          incr semi_move;
          `Ok (board_status (board, semi_move))
        end
      else
        `Invalid `InvalidMove

    let move board move =
      match board_status board with
      | `KeepPlaying -> actually_move board move
      | _ -> `Invalid `GameWasOver

    let serialize (board, _) =
      let result = Bytes.make 9 '-' in
      for i = 0 to 2 do
        for j = 0 to 2 do
          match board.(i).(j) with
          | Some piece -> Bytes.set result (3*i + j) (Piece.serialize piece)
          | None -> ()
        done
      done;
      Bytes.to_string result

    exception Deserialize_error

    let deserialize s =
      let board,_ = empty_board () in
      try
        for i = 0 to 2 do
          for j = 0 to 2 do
            match s.[3*i + j] with
            | '-' -> ()
            | c ->
               begin
                 match Piece.deserialize c with
                 | None -> raise Deserialize_error (* TODO: log error*)
                 | piece -> board.(i).(j) <- piece
               end
          done
        done;
        Some (board, ref 0)
      with
      | Deserialize_error -> None
  end
