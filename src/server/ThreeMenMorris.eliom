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

let last_square_clicked =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    (None : Ttt_game_lib_types.square option)

let move destination game user =
  let%lwt origin = Eliom_reference.get last_square_clicked in
  match origin with
  | None ->
     begin
       Logs.debug (fun m -> m "sending placement");
       let result =
         Game.place game destination.row destination.column user
       in
       let%lwt () =
         match result with
          | `Ok -> Lwt.return ()
          | _ -> Eliom_reference.set
                   last_square_clicked
                   (Some destination)
       in
       Lwt.return result
     end
  | Some(origin) ->
     begin
       Logs.debug (fun m -> m "sending move");
       let open Ttt_game_lib_types in
       let%lwt () = Eliom_reference.set last_square_clicked None in
       let result =
         Game.move game {origin; destination} user
       in
       Lwt.return result
     end

let click_square (game_id, row, column) =
  let%lwt user = Eliom_reference.get current_user in
  match user with
  | None -> Lwt.return (`Invalid `WrongPlayer)
  | Some (user, _) ->
     begin
       match Games.get_game (new id game_id) with
       | None -> (*Lwt.return (`Invalid `NoSuchId)*) assert false
       | Some (`TicTacToeClassical game) ->
          Logs.err (fun m -> m "Wrong game, should be 3 Morris!!!");
          let new_square =
            Ttt_game_lib_types.{row; column;} in
          move new_square game user
       | _ -> assert false
     end

let%client click_rpc =
  ~%(server_function [%derive.json: move_messages] click_square)

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

let%client position_to_square x y =
  let is_around x v =
    abs (x - v) <= 50
  in
  let (<=>) = is_around in

  let position_1d x =
    if x <=> 100 then
      Some(0)
    else if x <=> 300 then
      Some(1)
    else if x <=> 500 then
      Some(2)
    else
      None
  in
  match (position_1d x), (position_1d y) with
  | (Some x),(Some y) -> Some(x,y)
  | _ -> None


let%client init_client game_id =
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
  draw_piece `Black ~row:2 ~column:2 ctx;
  Lwt.async (fun () ->
      Lwt_js_events.mousedowns
        canvas
        (
          fun ev _ ->
          let x0, y0 = Dom_html.elementClientPosition canvas in
          let x,y = ((ev##.clientX - x0), (ev##.clientY - y0)) in
          Printf.printf "you clicked on (%d,%d)" x y;
          (match position_to_square x y with
           | Some(x,y) ->
              begin
                Printf.printf "which is square (%d,%d)\n" x y;
                click_rpc (game_id,x,y) >>=
                  (fun x -> Lwt.return (Some(x)))
              end
           | None ->
              begin
                Printf.printf "which is not a square\n";
                Lwt.return None
              end
          ) >>= (fun _ -> Lwt.return ())
        )
    )

let register () =
  let open Services in

  Base.TicTacToe_app.register
    ~service:ttt_3morris_service
    (fun id_int () ->
      let _ = [%client (init_client ~%id_int : unit)] in
      let () = init_board_canvas () in
      skeleton [
          div [
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
