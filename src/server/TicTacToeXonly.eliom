[%%shared
 open Eliom_lib
 open Eliom_content.Html.D
 open Lwt
 open Ttt_common_lib_types

 type move_messages =
   (int * int * int)
     [@@deriving json]

 type board_update = bool
 module Piece = Ttt_game_lib_pieces.XPiece
]

[%%server
 module Game = Common.TicTacToeXOnly
 module Games = Common.Games

 open Ttt_game_lib_types

 module TTTUsers = Ttt_user_lib_users

 let current_user = Common.current_user
]

let%shared (piece_to_string : Piece.t option -> string) = function
    None -> ""
  | Some(`X) -> "X"

let get_game id =
  match Games.get_game id with
  | None -> None
  | Some (`TicTacToeXOnly game) -> Some game
  | _ ->
     Logs.err (fun m -> m "game with id %d is not TTT X only" id#get_id); None

let move row column game user =
  let start_time = Core.Time.now () in
  let result = Game.place game ~row ~column user in
  let end_time = Core.Time.now () in
  let elapsed = Core.Time.abs_diff start_time end_time in
  Logs.debug (fun m -> m "Move executed in %s"
                         (Core.Time.Span.to_string elapsed)
             );
  Lwt.return result

let move (game_id, row, column) =
  let%lwt user = Eliom_reference.get current_user in
  match user with
  | None -> Lwt.return (`Invalid `WrongPlayer)
  | Some (user, _) ->
     begin
       match get_game (new id game_id) with
       | None -> (*Lwt.return (`Invalid `NoSuchId)*) assert false
       | Some game ->
          move row column game user
     end

let%client move_rpc = ~%(server_function Json.t<move_messages> move)

let refresh id_int =
  let game = get_game (new id id_int) in
  Lwt.return
    (match game with
      None -> Logs.warn (fun m -> m "Cannot refresh game, id doesn't exist")
    | Some game ->
       Game.refresh_game game)

let%client refresh = ~%(server_function Json.t<int> refresh)

let%client update_cell_content cell content =
  let dom_cell = Eliom_content.Html5.To_dom.of_element cell in
  ignore (React.E.map (fun c -> dom_cell##.innerHTML := Js.string c) content)

let%client cell_on_click dom_cell game_id x y =
  (Lwt.async (fun () ->
       Lwt_js_events.clicks
         dom_cell
         (fun _ _ ->
           let%lwt move_result = move_rpc (game_id, x, y) in
           begin
             match move_result with
               `Invalid `InvalidMove -> Eliom_lib.alert "Invalid move!"
             | `Invalid `NotYourTurn -> Eliom_lib.alert "It's not your turn!"
             | `Invalid `WrongPlayer -> Eliom_lib.alert "You are not playing in this game!"
             | `Invalid `GameWasOver -> Eliom_lib.alert "Game is over!"
             | _ ->  ()
           end;
           Lwt.return ())
     )
  )

let cell game_id x y
         (content : Piece.t option React.event) =
  let cell =
    td
      ~a:[a_class ["cell"]]
      []
  in
  let downcontent = Eliom_react.Down.of_react content in
  let game_id_int = game_id#get_id in
  let _ = [%client
              (let dom_cell = Eliom_content.Html5.To_dom.of_element ~%cell in
               cell_on_click dom_cell ~%game_id_int ~%x ~%y;
               update_cell_content ~%cell (React.E.map piece_to_string ~%downcontent)
               : unit)]
  in
  cell

let row id x =
  let game = match get_game id with
    | None -> failwith "impossible game"
    | Some g -> g
  in
  tr [
      cell id x 0 (Game.piece_at game ~row:x ~column:0);
      cell id x 1 (Game.piece_at game ~row:x ~column:1);
      cell id x 2 (Game.piece_at game ~row:x ~column:2)
     ]

let empty_row n = row n

let board_html game_id =
  table [
      row game_id 0;
      row game_id 1;
      row game_id 2
    ]

let skeleton  ?css:(css=[["css"; "TicTacToe.css"]]) ~title content =
  Base.skeleton
    ~css ~title content

let turn_sentence game user : string React.event =
  let user = match user with
      None -> ""
    | Some (user,_) -> user in
  let map = function
    | `GameOver (`Won player) -> player ^ " won the game!"
    | `GameOver `Drawn -> "Draw!"
    | `PlayOn f ->
       begin
         match f user with
         | `Play -> "It's your turn"
         | `Wait -> "It's your oponent's turn"
         | `Watch -> "Enjoy watching " ^ user
       end
  in
  React.E.map map (Game.game_status game)

type 'a game_page_result =
  | InvalidID
  | Content of 'a

let game_page game_id =
  match get_game game_id with
  | None -> Lwt.return InvalidID
  | Some game ->
     begin
       let phrase (user, piece) = (* TODO: rename this function *)
         Printf.sprintf "%s : %s" user (piece_to_string (Some piece))
       in
       let turn_sentence_div =
         div [pcdata ""]
       in
       let%lwt username =
         Eliom_reference.get current_user
       in
       let content =
         [
           div [h1 [pcdata "Welcome to this tic tac toe game!"]];
           div [
               pcdata (phrase (Game.username_and_piece game P1));
               br ();
               pcdata (phrase (Game.username_and_piece game P2))
             ];
           turn_sentence_div;
           div [board_html game_id(*; Chat_lib.chat_html ()*)];
         ] in
       let id_int = game_id#get_id in
       let ts = Eliom_react.Down.of_react (turn_sentence game username) in
       Logs.debug (fun m -> m "game_page %d about to send" game_id#get_id);
       let _ = [%client
                   (update_cell_content ~%turn_sentence_div ~%ts;
                    let%lwt () = refresh ~%id_int in
                    Lwt.return ()
                    : unit Lwt.t)
               ]
       in
       Logs.debug (fun m -> m "game_page %d returning" game_id#get_id);
       Lwt.map
         (fun x -> Content x)
         (skeleton
            ~css:[["css"; "TicTacToe.css"]]
            ~title:"Tic Tac Toe"
            content)
     end

let invalid_id_page () =
  skeleton
    ~css:[["css"; "TicTacToe.css"]]
    ~title:"Tic Tac Toe"
    [
      pcdata "This game doesn't exist. Go to your ";
      a Services.show_my_games_service [pcdata "game list"] ();
      pcdata " and chose a game from there."
    ]

let%client init_client () = ()

let options = {
    Eliom_registration.do_not_launch = false;
  }

let register () =
  let open Services in

  Base.TicTacToe_app.register
    ~service:ttt_xonly_service
    ~options
    (fun id_int () ->
      let _ = [%client (init_client () : unit)] in
      let%lwt page = game_page (new id id_int) in
      match page with
      | Content content ->
         Logs.debug (fun m -> m "sending game page content");
         Lwt.return content
      | InvalidID -> invalid_id_page ()
    )

let () = register ()
