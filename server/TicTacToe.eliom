[%%shared
    open Eliom_lib
    open Eliom_content.Html5.D
    open Lwt
    open Types

    module TTTUsers = Users.Users_test

    module TTT = User.TTT

    let _ = TTT.new_game "bruce" "bruce2"

    type messages =
      (int * int * int)
        [@@deriving json]

    type board_update = bool
]

module TicTacToe_app =
  Eliom_registration.App (
    struct
      let application_name = "TicTacToe"
    end)

let current_user = Common.current_user
let bus = Eliom_bus.create [%derive.json: string]

let%shared piece_to_string =
  let open Pieces.XOPiece in function
    None -> ""
  | Some(X) -> "X"
  | Some(O) -> "O"

let move (game_id, row, column) =
  let%lwt user = Eliom_reference.get current_user in
  match user with
  | None -> Lwt.return (`Invalid `WrongPlayer)
  | Some (user, _) ->
     begin
       Lwt.return (TTT.move (ID game_id) ~row ~column user)
     end

let%client move_rpc = ~%(server_function [%derive.json: messages] move)

let refresh id = TTT.refresh_game (ID id); Lwt.return ()
let%client refresh = ~%(server_function [%derive.json: int] refresh)

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

let cell (ID game_id) x y content =
  let cell =
    td
      ~a:[a_class ["cell"]]
      []
  in
  let downcontent = Eliom_react.Down.of_react content in
  let _ = [%client
              (let dom_cell = Eliom_content.Html5.To_dom.of_element ~%cell in
               cell_on_click dom_cell ~%game_id ~%x ~%y;
               update_cell_content ~%cell (React.E.map piece_to_string ~%downcontent)
               : unit)]
  in
  cell

let row id x =
  let game = match TTT.get_react_game_by_id id with
    | None -> failwith "impossible game"
    | Some g -> g
  in
  tr [
      cell id x 0 (TTT.piece_at game ~row:x ~column:0);
      cell id x 1 (TTT.piece_at game ~row:x ~column:1);
      cell id x 2 (TTT.piece_at game ~row:x ~column:2)
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

let welcome_page () =
  let content =
    [
      pcdata "Welcome! To start playing, click Play in the menu."
    ] in
  skeleton
    ~css:[["css"; "TicTacToe.css"]]
    ~title:"Board games"
    content

let show_my_games_page () =
  let%lwt current_user = Eliom_reference.get current_user in
  let create_game_link =
    div [a Services.input_create_game_service [pcdata "create new game"] ()] in
  let game_list =
    match current_user with
      None -> div [pcdata "Log in to save your games"]
    | Some (user, _) ->
       let idgame_to_link (ID id,game) =
         a Services.ttt_service [pcdata (string_of_int id)] id in
       let games = TTT.get_current_games user in
       match games with
         [] -> div [pcdata "You have no games in progress, start a new one to play."]
       | games ->
          let links = List.map idgame_to_link games in
          let bullets = List.map (fun link -> li [link]) links in
          div [ul bullets]
  in
  skeleton
    ~css:[["css"; "TicTacToe.css"]]
    ~title:"Game list"
    [create_game_link; game_list]

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
  React.E.map map (TTT.game_status game)

type 'a game_page_result =
  | InvalidID
  | Content of 'a

let game_page game_id =
    match TTT.get_react_game_by_id game_id with
    | None -> Lwt.return InvalidID
    | Some g ->
       begin
         let game = g in

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
                 pcdata (phrase (TTT.username_and_piece game_id P1));
                 br ();
                 pcdata (phrase (TTT.username_and_piece game_id P2))
               ];
             turn_sentence_div;
             div [board_html game_id; Chat_lib.chat_html ()];
           ] in
         let ts = Eliom_react.Down.of_react (turn_sentence game username) in
         let _ = [%client
                     (update_cell_content ~%turn_sentence_div ~%ts;
                      let ID id = ~%game_id in
                      let%lwt () = refresh id in
                      Lwt.return ()
                      : unit Lwt.t)
                 ]
         in
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
    [pcdata
       ("This game doesn't exist. Go to your game list" ^
          " and chose a game from there.")]

let create_game_form user =
  [div
     [
       pcdata "Play against: ";
       Form.input ~input_type:`Text ~name:user Form.string;
       Form.input ~input_type:`Submit ~value:"Challenge" Form.string
     ]
  ]

let create_game_page () =
  let form = Form.get_form Services.create_game_service create_game_form in
  skeleton
    ~title:"Tic Tac Toe --- create game"
    [div [form]]


let%client init_client () = ()

let options = {
    Eliom_registration.do_not_launch = false;
  }

let register () =
  let open Services in
  Eliom_registration.Html5.register
    ~service:main_service
    (fun () () ->
      welcome_page ()
    );

  TicTacToe_app.register
    ~service:ttt_service
    ~options
    (fun id () ->
      let _ = [%client (init_client () : unit)] in
      let%lwt page = game_page (ID id) in
      match page  with
      | Content content -> Lwt.return content
      | InvalidID -> invalid_id_page ()
    );

  Eliom_registration.Html5.register
    ~service:input_create_game_service
    (fun () () -> (create_game_page ()));

  Eliom_registration.Redirection.register
    ~service:create_game_service
    ~options:`TemporaryRedirect
    (fun user_name () ->
      let%lwt current_user = Eliom_reference.get current_user in
      match current_user with
      | None -> failwith "no good"
      | Some (user, _) ->
         let ((ID id),game) = TTT.new_game user_name user in
         let game_service = Eliom_service.preapply ttt_service id in
         Lwt.return game_service);

  TicTacToe_app.register
    ~service:show_my_games_service
    (fun () () -> show_my_games_page ())

let () = register ()
