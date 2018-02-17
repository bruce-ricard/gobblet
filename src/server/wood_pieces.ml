open Eliom_content.Html5.D

let make_piece piece_id () =
  span
    ~a:[
      a_class (piece_id :: ["wood-piece"]);
    ]
    []

let white_piece1 =
  make_piece "white-piece1"

let black_piece1 =
  make_piece "black-piece1"
