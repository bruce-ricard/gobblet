[%%shared
    open Eliom_lib
    open Eliom_content
    open Html5
    open Html5.D
    open Lwt
    open Ttt
    open Users

    module XOBoard = Board(XOPiece)

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
  Eliom_service.App.service
    ~path:[]
    ~get_params:Eliom_parameter.unit
    (*    ~https:true*)
    ()

let connection_service =
  Eliom_service.Http.post_service
    ~fallback:main_service
    ~post_params:Eliom_parameter.(string "name" ** string "password")
        ()

let board = ref (XOBoard.empty_board ())
let bus = Eliom_bus.create [%derive.json: string]

let update_board new_board =
  board := new_board

let%client current_player = ref P1
let%client update_current_player () =
  if !current_player = P1 then
    current_player := P2
  else
    current_player := P1

let%client update_cells_matrix = Array.make_matrix 3 3 (fun (s : string) -> ())

let%shared position_to_string = function
    None -> ""
  | Some(p) -> XOPiece.to_string p

let%client update_game board =
  for x = 0 to 2 do
    for y = 0 to 2 do
      update_cells_matrix.(x).(y) (position_to_string board.(x).(y))
    done
  done

let move (row, column, player) =
  Lwt.wrap (fun () ->
      let result = XOBoard.move !board ~row ~column (Obj.magic player) in
      begin
        let open XOBoard in
        match result with
          InvalidMove -> ()
        | Next(_,new_board) -> board := new_board
      end;
      result
    )


let%client move_rpc =  ~%(server_function [%derive.json: messages] move)

let cell x y =
  let cell =
    td
      ~a:[a_class ["cell"]]
      [pcdata ""] (* (position_to_string board.(x).(y))]*)
  in
  let _ = [%client
              (Lwt.async (fun () ->
                   let dom_cell = Eliom_content.Html5.To_dom.of_element ~%cell in
                   update_cells_matrix.(~%x).(~%y) <-
                     (fun s -> dom_cell##.innerHTML := Js.string s);
                   Lwt_js_events.clicks
                     dom_cell
                     (fun _ _ ->
                       let open XOBoard in
                       (
                         let%lwt move_result = move_rpc (~%x,~%y, Obj.magic !current_player) in
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
                 )
               : unit)]
  in
  cell

let row x =
  tr [cell x 0 ; cell x 1; cell x 2]

let empty_row n = row n

let board_html () =
  table [
      empty_row 0;
      empty_row 1;
      empty_row 2
    ]

let reset_game () =
  board := XOBoard.empty_board ();
  Lwt.return ()

let%client reset_game_rpc = ~%(server_function [%derive.json: unit] reset_game)

let new_game_button () =
  let elt = div [button [pcdata "New game"]] in
  [%client
      ((Lwt.async (fun () ->
            let dom_elt = Eliom_content.Html5.To_dom.of_element ~%elt in
            Lwt_js_events.clicks
              dom_elt
              (fun _ _ -> reset_game_rpc ())))
       : unit)
  ];
  elt

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

let username =
  Eliom_reference.eref ~scope:Eliom_common.default_session_scope None

let connection_box () =
  let%lwt u = Eliom_reference.get username in
    Lwt.return
      (match u with
       | Some s -> p [pcdata "You are connected as "; pcdata s]
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

let header_login () =
  let%lwt username = Eliom_reference.get username in
  match username with
  | Some name -> Lwt.return (pcdata ("Logged in as " ^ name))
  | None -> connection_box ()

let header () =
  let%lwt login = header_login () in
  Lwt.return (
      div ~a:[a_class ["header"]]
          [
            div ~a:[a_class ["logo"]] [
                  div ~a:[a_class ["logo_image"]] [pcdata "put logo here"];
                  div ~a:[a_class ["site_name"]] [pcdata "Online board games"]
                ];
            div ~a:[a_class ["header_login"]] [login]
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

let page () =
  let%lwt cb = connection_box () in
  let content =
    [
      div [h1 [pcdata "Welcome to this tic tac toe game!"]];
      new_game_button ();
      div [board_html (); chat_html ()];
      cb;
    ] in
  skeleton
    ~css:[["css"; "TicTacToe.css"]]
    ~title:"Tic Tac Toe"
    content



let%client init_client () = ()

(* User names and passwords: *)
let users = ref [("Calvin", "123"); ("Hobbes", "456")]

let check_pwd name pwd =
    try List.assoc name !users = pwd with Not_found -> false

let options = {
    Eliom_registration.do_not_launch = true;
  }

let () =
  TicTacToe_app.register
    ~service:main_service
    ~options
    (*    ~scope:Eliom_common.default_group_scope*)
    (fun () () ->
      let _ = [%client (init_client () : unit)] in
      page ()
    );

  Eliom_registration.Action.register
    ~service:connection_service
    (fun () (name, password) ->
      if check_pwd name password then
        begin
          Eliom_reference.set username (Some name);
          Lwt.return ()
        end
      else
        Lwt.return ()
    )
