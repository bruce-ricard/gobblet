open Ttt_game_lib_types

module Make (W : WINNER_WINS) (Piece : PIECE)
       : BOARD with type piece = Piece.t
  =
  struct
    type piece = Piece.t

    type t = Piece.t option array array
    let pieces = Piece.pieces
    let empty_board () : t = Array.make_matrix 3 3 None

    let piece_at board ~row ~column =
      board.(row).(column)

    let square_is_occupied board {row; column} =
      match piece_at board ~row ~column with
      | Some _ -> true
      | None -> false

    let square_is_free board square =
      not @@ square_is_occupied board square

    let valid_placement board ~row ~column =
      square_is_free board {row; column}

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
          board
          {origin; destination}
      =
      (square_is_occupied board origin)
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

    let check_board (board : t) : Piece.t option =
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

    let board_status board =
      match check_board board with
      | None ->
         if board_is_full board then
          `Draw
         else
           `KeepPlaying
      | Some piece ->  if W.wins then `Won else `Lost

    let actually_place board ~row ~column piece  =
      if valid_placement board ~row ~column then
        begin
          board.(row).(column) <- Some piece;
          `Ok (board_status board)
        end
      else
        `Invalid `InvalidMove

    let place board {row; column} piece =
      match board_status board with
      | `KeepPlaying -> actually_place board ~row ~column piece
      | _ -> `Invalid `GameWasOver

    let actually_move board move =
      if valid_move board move then
        begin
          let {origin; destination} = move in
          let piece = board.(origin.row).(origin.column) in
          board.(origin.row).(origin.column) <- None;
          board.(destination.row).(destination.column) <- piece;
          `Ok (board_status board)
        end
      else
        `Invalid `InvalidMove

    let move board move =
      match board_status board with
      | `KeepPlaying -> actually_move board move
      | _ -> `Invalid `GameWasOver

    let serialize board =
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
      let board = empty_board () in
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
        Some board
      with
      | Deserialize_error -> None
  end
