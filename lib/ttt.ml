type player = P1 | P2

module type PIECE =
  sig
    type t
    val piece_of : player -> t
    val to_string : t -> string
    val example : int -> t
  end

module Board (Piece : PIECE) =
  struct
    type board = Piece.t option array array
    let empty_board () : board = Array.make_matrix 3 3 None
    let example () =
      let board = empty_board () in
      for x = 0 to 2 do
        for y = 0 to 2 do
          board.(x).(y) <- Some (Piece.example (3*x + y + 1))
        done
      done;
      board

    let to_ascii_art board =
      let piece_option_to_string = function
          None -> " "
        | Some piece -> Piece.to_string piece
      in
      let row_to_art row =
        piece_option_to_string row.(0) ^ " | " ^
          piece_option_to_string row.(1) ^ " | " ^
            piece_option_to_string row.(2)
      in
      row_to_art board.(0) ^ "\n" ^ "_________\n" ^
      row_to_art board.(1) ^ "\n" ^ "_________\n" ^
      row_to_art board.(2) ^ "\n"

    let print board = print_string (to_ascii_art board)

    let valid_move board ~row ~column player = true

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

    let check_board (board : board) : Piece.t option =
      List.fold_left (fun result element ->
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

    type result = KeepPlaying | Won of player | Draw

    type move_result =
      | InvalidMove
      | Next of result * board

    let move board ~row ~column player =
      if valid_move board ~row ~column player then
        begin
          let board = copy_board board in
          board.(row).(column) <- Some (Piece.piece_of player);
          match check_board board with
            None ->
            if board_is_full board then
              Next(Draw, board)
            else
              Next(KeepPlaying, board)
          | Some piece -> Next(Won player, board)
        end
      else
        InvalidMove

    let move_unsafe board ~row ~column player =
      match move board ~row ~column player with
        InvalidMove -> failwith "Invalid Move"
      | Next(result, board) -> result, board


  end

module IntPiece =
  struct
    type t = int
    let to_string = string_of_int
    let example x = x
    let piece_of = function
        P1 -> 1 | P2 -> 2
  end

module XOPiece =
  struct
    type t = X | O
    let to_string = function
        X -> "X"
      | O -> "O"

    let example x = X
    let piece_of = function
        P1 -> X | P2 -> O
  end
