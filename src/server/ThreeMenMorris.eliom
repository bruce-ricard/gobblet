[%%shared
 open Eliom_lib
 open Eliom_content.Html.D
 open Eliom_content

 open Ttt_common_lib_types

 type move_messages =
   (int * int * int)
     [@@deriving json]

 module Piece = Ttt_game_lib_pieces.XOPiece
]

[%%server
 module Game = Common.ThreeMenMorris
 module Games = Common.Games

 open Ttt_game_lib_types

 module TTTUsers = Common.Users

 let current_user = Common.current_user
]

let last_square_clicked =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    (None : Ttt_game_lib_types.square option)

let get_game id : Common.ThreeMenMorris.game option =
  match Games.get_game id with
  | None -> None
  | Some (`ThreeMenMorris game) -> Some game
  | _ ->
     begin
       Logs.err
         (fun m -> m "game with id %d is not Three Men Morris!"
                     id#get_id);
       None
     end

let refresh id_int =
  let game = get_game (new id id_int) in
  Lwt.return
    (match game with
       None -> Logs.warn
                 (fun m -> m "Cannot refresh game, id doesn't exist")
    | Some game ->
       Game.refresh_game game)

let refresh = Eliom_client.server_function [%json: int] refresh

let%client refresh = ~%refresh

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
       | Some (`ThreeMenMorris game) ->
          let new_square =
            Ttt_game_lib_types.{row; column;} in
          move new_square game user
       | _ -> Logs.err (fun m -> m "Wrong game, should be 3 Morris!!!");
              assert false
     end

let%client click_rpc =
  ~%(Eliom_client.server_function [%json: move_messages] click_square)

let skeleton  ?css:(css=[["css"; "ThreeMorris.css"]])
              ?title:(title="Three men Morris")
              content =
  Tttbase.skeleton
    ~css ~title content

let%client draw ctx ((x1, y1), (x2, y2)) =
  let open Js_of_ocaml in
  let color = CSS.Color.string_of_t (CSS.Color.rgb 1 2 3) in
  ctx##.strokeStyle := (Js.string color);
  ctx##.lineWidth := float 10;
  ctx##beginPath;
  ctx##(moveTo (float x1) (float y1));
  ctx##(lineTo (float x2) (float y2));
  ctx##stroke;
  ()

let%client draw_dot ctx x y =
  let open Js_of_ocaml in
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
      img##.onload := Html.handler (fun _ -> c (); Js_of_ocaml.Js._false);
      img##.src := src;
    )
  >>= fun () -> Lwt.return img

let%client clear_piece ~row ~column ctx =
  let x = float_of_int (row * 200 + 50)
  and y = float_of_int (column * 200 + 50)
  and width = 100.
  and height = 100.
  in
  ctx##clearRect x y width height

let%client draw_piece color ~row ~column ctx =
  let draw sprite color =
    let sourceX, sourceY = match color with
      | `White -> (0., 0.)
      | `Black -> (0., 128.)

    and sourceWidth = 64.
    and sourceHeight = 64.
    and destX = float_of_int (row * 200 + 50)
    and destY = float_of_int (column * 200 + 50)
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


let%client draw_board ctx =
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

let%client piece_to_color = function
    `X -> `White | `O -> `Black

let%client update_pieces ctx piece_events =
  let update ~row ~column piece =
    match piece with
    | None ->
       clear_piece ~row ~column ctx
    | Some piece ->
       draw_piece
         ~row
         ~column
         (piece_to_color piece)
         ctx
  in
  for row = 0 to 2 do
    for column = 0 to 2 do
      ignore (React.E.map
                (update ~row ~column)
                piece_events.(row).(column)
             )
    done
  done

let board_canvas_elt game_id =
  let game_id = game_id#get_id in
  let elt =
    Html.D.canvas
      ~a:[
        a_id "board_canvas";
        Html.D.a_width 600;
        Html.D.a_height 600;
      ]
      [
        Html.D.pcdata "your browser doesn't support canvas";
        Html.D.br ();
      ]
  in
  let _ = [%client
              ((let canvas = Html.To_dom.of_canvas ~%elt in
                let st = canvas##.style in
                st##.zIndex := Js_of_ocaml.Js.string "2";
                let ctx = canvas##(getContext (Dom_html._2d_)) in
                ctx##.lineCap := Js_of_ocaml.Js.string "round";
                draw_board ctx;
                Lwt.async (fun () ->
                    Js_of_ocaml_lwt.Lwt_js_events.mousedowns
                      canvas
                      (
                        fun ev _ ->
                        let x0, y0 = Dom_html.elementClientPosition canvas in
                        let x,y = ((ev##.clientX - x0), (ev##.clientY - y0)) in
                        Printf.printf "you clicked on (%d,%d)" x y;
                        (match position_to_square x y with
                         | Some(x,y) ->
                            begin
                              Printf.printf " which is square (%d,%d)\n" x y;
                              click_rpc (~%game_id,x,y) >>=
                                (fun x -> Lwt.return (Some(x)))
                            end
                         | None ->
                            begin
                              Printf.printf "which is not a square\n";
                              Lwt.return None
                            end
                        ) >>= (fun _ -> Lwt.return ())
                      )
                  );
               ) : unit)
          ]
  in elt

let piece_events game
    : Piece.t option Eliom_react.Down.t array array =

  let board =
    Array.make_matrix
      3 3
      (Eliom_react.Down.of_react React.E.never)
  in
  for row = 0 to 2 do
    for column = 0 to 2 do
      board.(row).(column) <-
        Eliom_react.Down.of_react
          (Game.piece_at game ~row ~column)
    done
  done;
  board

let pieces_canvas_elt game =
  let elt =
    Html.D.canvas
      ~a:[
        a_id "pieces_canvas";
        Html.D.a_width 600;
        Html.D.a_height 600;
      ]
      [
        Html.D.pcdata "your browser doesn't support canvas";
        Html.D.br ();
      ]
  in
  let events = piece_events game
  in
  let _ = [%client
              (let canvas = Html.To_dom.of_canvas ~%elt in
               let st = canvas##.style in
               st##.zIndex := Js_of_ocaml.Js.string "3";
               st##.pointerEvents := Js_of_ocaml.Js.string "none";
               let ctx = canvas##(getContext (Dom_html._2d_)) in
               ctx##.lineCap := Js_of_ocaml.Js.string "round";
               update_pieces ctx ~%events : unit)
          ]
  in
  elt

let%client init_client game_id board_event =
  refresh game_id

type 'a game_page_result =
  | InvalidID
  | Content of 'a

let%client update_html_content elt content =
  let dom_html = Eliom_content.Html.To_dom.of_element elt in
  ignore (React.E.map
            (fun c -> dom_html##.innerHTML := Js_of_ocaml.Js.string c)
            content)

let game_page game_id =
  match get_game game_id with
  | None -> Lwt.return InvalidID
  | Some game ->
     begin
       let rating user =
         match TTTUsers.formatted_rating
                 user
                 `ThreeMenMorris
         with
         | None -> (Logs.err (fun m -> m "No rating for user %s" user);
                    "-")
         | Some s -> s
       in
       let phrase () =
         let piece_to_string = function
           | `X -> "White"
           | `O -> "Black"
         in
         let (user1, piece1) = Game.username_and_piece game P1
         and (user2, piece2) = Game.username_and_piece game P2 in
         let u1rating = rating user1
         and u2rating = rating user2 in

         (* TODO: rename this function *)
         Printf.sprintf
           "%s (%s) (%s) Vs. %s (%s) (%s)"
           user1
           u1rating
           (piece_to_string piece1)
           user2
           u2rating
           (piece_to_string piece2)
       in
       let turn_sentence_div =
         div [pcdata ""]
       in
       let%lwt username =
         Eliom_reference.get current_user
       in
       let content =
         [
           div [h1 [pcdata "Play Three Men Morris."]];
           div [
               pcdata (phrase ())
             ];
           turn_sentence_div;
           div ~a:[
                 a_id "board";
               ]
               [
                 board_canvas_elt game_id;
                 pieces_canvas_elt game;
               ];
         ]
       in
       let id_int = game_id#get_id in
       let ts = Eliom_react.Down.of_react
                  (Game_page_common.turn_sentence
                     (Game.game_status game)
                     username) in
       Logs.debug
         (fun m -> m "game_page %d about to send"
                     game_id#get_id);
       let _ = [%client
                   (update_html_content
                      ~%turn_sentence_div
                      ~%ts;
                    let%lwt () = refresh ~%id_int in
                    Lwt.return ()
                    : unit Lwt.t)
               ]
       in
       Logs.debug (fun m -> m "game_page %d returning" game_id#get_id);
       Lwt.map
         (fun x -> Content x)
         (skeleton
            ~css:[["css"; "ThreeMorris.css"]]
            ~title:"Three men Morris"
            content)
     end

let register () =
  let options = {
      Eliom_registration.do_not_launch = false;
    } in

  let open Services in

  Tttbase.TicTacToe_app.register
    ~service:ttt_3morris_service
    ~options
    (fun id_int () ->
      let id = new id id_int in
      let%lwt page = game_page id in
      match page with
      | Content content ->
         Logs.debug (fun m -> m "sending 3 morris game page content");
         let game =
           match get_game id with
           | Some game -> game
           | None -> assert false
         in
         let board_events = piece_events game in
         let _ = [%client
                     (Lwt.async (fun () ->
                          init_client ~%id_int ~%board_events)
                      : unit)
                 ] in
         Lwt.return content
      | InvalidID ->
         Invalid_id_page.html ()
    )

let () = register ()
