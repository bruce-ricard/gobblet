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
  ctx##.lineWidth := float 40;
  ctx##beginPath;
  ctx##(arc (float x) (float y) 1. 0. 0.1 (Js.bool true));
  ctx##stroke;
  ()

[%%client
 module Html = Dom_html
]

let%client lwt_wrap f =
  let (t, w) = Lwt.task () in
  let cont x = Lwt.wakeup w x in
  f cont;
  t

let%client load_image src =
  let img = Html.createImg Html.document in
  lwt_wrap
    (fun c ->
      img##.onload := Html.handler (fun _ -> c (); Js._false);
      img##.src := src;
    )
  >>= fun () -> Lwt.return img

let%client draw_piece color ~row ~column ctx =
  let draw sprite color =
    let sourceX, sourceY = match color with
      | `White -> (0., 0.)
      | `Black -> (0., 128.)

    and sourceWidth = 64.
    and sourceHeight = 64.
    and destX = float_of_int (column * 200 + 50)
    and destY = float_of_int (row * 200 + 50)
    and destWidth = 100.
    and destHeight = 100.
    in

    ctx##drawImage_full
      sprite
      sourceX
      sourceY
      sourceWidth
      sourceHeight
      destX
      destY
      destWidth
      destHeight
  in

  let piece_sprite =
    load_image (Js.string "/games/pieces.png") in

  Lwt.async (
      fun () ->
      piece_sprite >>= (
        fun sprite ->
        draw sprite color;
        Lwt.return ()
      )
    )

let init_board_canvas () = ()

let%client init_client () =
  let canvas = Html5.To_dom.of_canvas ~%canvas_elt in
  let st = canvas##.style in
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
  draw_dot ctx 500 500;
  draw_piece `White ~row:0 ~column:1 ctx;
  draw_piece `White ~row:1 ~column:1 ctx;
  draw_piece `Black ~row:2 ~column:2 ctx

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
              div ~a:[
                    a_id "board";
                  ]
                  [
                    canvas_elt;
                    span ~a:[a_class ["clear"]] [];
                  ];
            ]
        ]
    )

let () = register ()
