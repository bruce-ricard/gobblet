[%%shared
 open Eliom_lib.Lwt_ops
 open Eliom_content.Html5.D
 open Eliom_content

 open Lwt
 open Ttt_common_lib_types

 type move_messages =
   (int * int * int)
     [@@deriving json]

 type board_update = bool
 module Piece = Ttt_game_lib_pieces.XOPiece
]

[%%server
 module Game = Common.TicTacToeClassical
 module Games = Common.Games

 open Ttt_game_lib_types

 module TTTUsers = Common.Users

 let current_user = Common.current_user
]

(* jessica *)

let skeleton  ?css:(css=[["css"; "ThreeMorris.css"]])
              ?title:(title="Three men Morris")
              content =
  Base.skeleton
    ~css ~title content

let canvas_elt =
  Html5.D.canvas
    ~a:[
      Html5.D.a_width 600;
      Html5.D.a_height 600;
    ]
    [
      Html5.D.pcdata "your browser doesn't support canvas";
      Html5.D.br ();
    ]

let%client draw ctx ((x1, y1), (x2, y2)) =
  let color = CSS.Color.string_of_t (CSS.Color.rgb 1 2 3) in
  ctx##.strokeStyle := (Js.string color);
  ctx##.lineWidth := float 10;
  ctx##beginPath;
  ctx##(moveTo (float x1) (float y1));
  ctx##(lineTo (float x2) (float y2));
  ctx##stroke;
  ()

let%client draw_dot ctx x y =
  let color = CSS.Color.string_of_t (CSS.Color.rgb 1 2 3) in
  ctx##.strokeStyle := (Js.string color);
  ctx##.lineWidth := float 20;
  ctx##beginPath;
  ctx##(arc (float x) (float y) 1. 0. 0.1 (Js.bool true));
  ctx##stroke;
  ()

let init_board_canvas () = ()

let%client init_client () =
  let canvas = Html5.To_dom.of_canvas ~%canvas_elt in
  let st = canvas##.style in
  st##.position := Js.string "absolute";
  st##.zIndex := Js.string "-1";
  let ctx = canvas##(getContext (Dom_html._2d_)) in
  ctx##.lineCap := Js.string "round";
  draw ctx ((100,100),(100,500));
  draw ctx ((100,100),(500,100));
  draw ctx ((100,500),(500,500));
  draw ctx ((500,100),(500,500));
  draw ctx ((100,300),(500,300));
  draw ctx ((300,100),(300,500));
  draw ctx ((100,100),(500,500));
  draw ctx ((100,500),(500,100));
  draw_dot ctx 100 100;
  draw_dot ctx 100 300;
  draw_dot ctx 100 500;
  draw_dot ctx 300 100;
  draw_dot ctx 300 300;
  draw_dot ctx 300 500;
  draw_dot ctx 500 100;
  draw_dot ctx 500 300;
  draw_dot ctx 500 500

let register () =
  let open Services in

  Base.TicTacToe_app.register
    ~service:ttt_3morris_service
    (fun id_int () ->
      let _ = [%client (init_client () : unit)] in
      let () = init_board_canvas () in
      skeleton [
          div [
              pcdata "3 men morris page";
              br ();
              div ~a:[a_id "board"] [
                    canvas_elt;
                    span ~a:[a_class ["clear"]] [];
                  ];
              let open Wood_pieces in
              div
                [
                  white_piece1 ();
                  black_piece1 ();
                  br ();
                  black_piece1 ();
                  white_piece1 ();
                ]
            ]
        ]
    )

let () = register ()
