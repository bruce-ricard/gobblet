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

    let valid_move board ~row ~column =
      match board.(row).(column) with
        None -> true
      | Some(_) -> false

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

    let actually_move board ~row ~column piece  =
      if valid_move board ~row ~column then
        begin
          board.(row).(column) <- Some piece;
          `Ok (board_status board)
        end
      else
        `Invalid `InvalidMove

    let move board ~row ~column piece =
      match board_status board with
        `KeepPlaying -> actually_move board ~row ~column piece
      | _ -> `Invalid `GameWasOver

    let move_unsafe board ~row ~column piece =
      match move board ~row ~column piece with
        `InvalidMove -> failwith "Invalid Move"
      | result -> result

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
