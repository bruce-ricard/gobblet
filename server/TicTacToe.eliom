[%%shared
    open Eliom_lib
    open Eliom_content
    open Html5
    open Html5.D
    open Lwt
    open Ttt
    open Users
    open User

    module TTTUsers = Users_test

    (*    module TTTGame = TTTGameF(Ttt.XOPiece)*)
    module TTT = TTTGameInProgress

    type messages =
      (int * int * int)
        [@@deriving json]
]

module TicTacToe_app =
  Eliom_registration.App (
    struct
      let application_name = "TicTacToe"
    end)


let main_service =
  Eliom_service.Http.service
    ~path:[]
    ~get_params:Eliom_parameter.unit
    (*    ~https:true*)
    ()

let show_my_games_service =
  Eliom_service.Http.service
    ~path:[]
    ~get_params:Eliom_parameter.unit
    ()

let ttt_service =
  Eliom_service.App.service
    ~path:["games"; "tictactoe"]
    ~get_params:Eliom_parameter.(int "game_id")
    ()

let connection_service =
  Eliom_service.Http.post_coservice'
    ~post_params:Eliom_parameter.(string "name" ** string "password")
    ()

let disconnection_service =
  Eliom_service.Http.post_coservice'
    ~post_params:Eliom_parameter.unit
    ()

let bus = Eliom_bus.create [%derive.json: string]
let current_user =
  Eliom_reference.eref ~scope:Eliom_common.default_session_scope (None : (string * user) option)

let%client update_cells_matrix = Array.make_matrix 3 3 (fun (s : string) -> ())

let%shared piece_to_string =
  let open XOPiece in function
    None -> ""
  | Some(X) -> "X"
  | Some(O) -> "O"

let%client update_game game =
  for row = 0 to 2 do
    for column = 0 to 2 do
      update_cells_matrix.(row).(column) (piece_to_string (TTT.piece_at game ~row ~column))
    done
  done

let move (game_id, row, column) =
  let%lwt user = Eliom_reference.get current_user in
  match user with
  | None -> Lwt.return `WrongPlayer
  | Some (user, _) ->
     begin
       match TTTGames.get_game_by_id (ID game_id) with
         None -> Lwt.return `WrongPlayer
       | Some game ->
          Lwt.wrap (fun () ->
              let result = TTT.move game ~row ~column user in
              result)
     end

let%client move_rpc =  ~%(server_function [%derive.json: messages] move)

let cell game_id x y content =
  let sign = piece_to_string content in
  let cell =
    td
      ~a:[a_class ["cell"]]
      [pcdata sign]
  in
  let _ = [%client
              (Lwt.async (fun () ->
                   let dom_cell = Eliom_content.Html5.To_dom.of_element ~%cell in
                   update_cells_matrix.(~%x).(~%y) <-
                     (fun s -> dom_cell##.innerHTML := Js.string s);
                   Lwt_js_events.clicks
                     dom_cell
                     (fun _ _ ->
                       let%lwt move_result = move_rpc (~%game_id, ~%x,~%y) in
                       begin
                         match move_result with
                           InvalidMove -> Eliom_lib.alert "Invalid move!"
                         | Next(result, new_board) ->
                            begin
                              update_game new_board;

                              match result with
                                KeepPlaying ->  update_current_player ()
                              | Won P1 -> Eliom_lib.alert "Player 1 won !"
                              | Won P2 -> Eliom_lib.alert "Player 2 won !"
                              | Draw -> Eliom_lib.alert " Draw!"
                            end
                       end;
                       Lwt.return ())
                 );
            : unit)]
in
  cell

let row id game x =
  tr [cell id x 0 (TTT.piece_at game ~row:x ~column:0);
      cell id x 1 (TTT.piece_at game ~row:x ~column:1);
      cell id x 2 (TTT.piece_at game ~row:x ~column:2)
     ]

let empty_row n = row n

let board_html game_id =
  let game =
    match TTTGames.get_game_by_id game_id with
    | Some g -> g
    | None -> failwith "no game"
  in
  (* TODO populate with game instead of empty*)
  table [
      row game_id game 0;
      row game_id game 1;
      row game_id game 2
    ]

let chat_logs_html () =
  ul [
      li [pcdata "test"];
      li [pcdata "123"]
    ]

let chat_input_text_html () =
  div []

let chat_html () =
  let elt =
    div [
        h3 [pcdata "Chat window"];
        chat_logs_html ();
        chat_input_text_html ()
      ] in
  elt

let connection_box () =
  let%lwt user = Eliom_reference.get current_user in
    Lwt.return
      (match user with
       | Some (user,_) -> p [pcdata "You are connected as "; pcdata user]
       | None ->
          Form.post_form ~service:connection_service
                         (fun (name1, name2) ->
                           [fieldset
                              [label [pcdata "login: "];
                               Form.input
                                 ~input_type:`Text ~name:name1
                                 Form.string;
                               br ();
                               label [pcdata "password: "];
                               Form.input
                                 ~input_type:`Password ~name:name2
                                 Form.string;
                               br ();
                               Form.input
                                 ~input_type:`Submit ~value:"Connect"
                                 Form.string
                         ]])
                         ()
      )

let disconnect_box () =
  Form.post_form
    ~service:disconnection_service
    (
      fun () ->
      [Form.input ~input_type:`Submit ~value:"Log out" Form.string]
    )
    ()

let header_login () =
  let%lwt current_user = Eliom_reference.get current_user in
  match current_user with
  | Some (name, _) -> Lwt.return (
                     div
                       [
                         pcdata ("Logged in as " ^ name);
                         disconnect_box ()
                       ]
                   )
  | None -> connection_box ()

let header () =
  let%lwt login = header_login () in
  let menu =
    div ~a:[a_class ["menu"]]
        [
          a ttt_service [pcdata "Tic Tac Toe"] 1
        ]
  in
  Lwt.return (
      div
        [
          div ~a:[a_class ["header"]]
              [
                a main_service
                  [
                    div ~a:[a_class ["logo"]] [
                          div ~a:[a_class ["logo_image"]] [pcdata "put logo here"];
                          div ~a:[a_class ["site_name"]] [pcdata "Online board games"]
                        ];
                  ] ();
                div ~a:[a_class ["header_login"]] [login]
              ];
          menu
        ]
    )

let skeleton ~css ~title content =
  let%lwt header = header () in
  Lwt.return
    (html
     (Eliom_tools.F.head
        ~css ~title
        ()
     )
     (body
        (header :: content)
     )
    )

let welcome_page () =
  let%lwt cb = connection_box () in
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
  let content =
    match current_user with
      None -> div [pcdata "Log in to save your games"]
    | Some (_,user) ->
       let games = user#get_games in
       ul (List.map (fun (id,game) -> li [pcdata "nada"]) games)
          (* TODO add links towards all games *)
  in
  skeleton
    ~css:[["css"; "TicTacToe.css"]]
    ~title:"Game list"
    [content]



let game_page game =
  let content =
    [
      div [h1 [pcdata "Welcome to this tic tac toe game!"]];
      div [board_html game; chat_html ()];
    ] in
  let _ = [%client update_game game] in
  skeleton
    ~css:[["css"; "TicTacToe.css"]]
    ~title:"Tic Tac Toe"
    content

let%client init_client () = ()

let options = {
    Eliom_registration.do_not_launch = false;
  }

let () =
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
      game_page (ID id)
    );

  Eliom_registration.Html5.register
    ~service:show_my_games_service
    (fun () () -> show_my_games_page ());

  Eliom_registration.Action.register
    ~service:connection_service
    (fun () (name, password) ->
      match TTTUsers.log_in name password with
        None -> Lwt.return ()
      | Some user -> Eliom_reference.set current_user (Some(name,user))
    );

  Eliom_registration.Action.register
    ~service:disconnection_service
    (fun () () ->
      (Eliom_reference.set current_user (None));
    )
