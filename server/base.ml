open Eliom_content.Html5.D

let _ = let open Common in let open Connection_code in ()

let header_login () =
  let%lwt current_user = Eliom_reference.get Common.current_user in
  match current_user with
  | Some (name, _) -> Lwt.return (
                     div
                       [
                         pcdata ("Logged in as " ^ name);
                         Connection_code.disconnect_box ()
                       ]
                   )
  | None -> Connection_code.connection_box ()

let header () =
  let%lwt login = header_login () in
  let menu =
    div ~a:[a_class ["menu"]]
        [
          a Services.show_my_games_service [pcdata "Tic Tac Toe"] ()
        ]
  in
  let contents =
    [
      div ~a:[a_class ["header"]]
          [
            a Services.main_service
              [
                div ~a:[a_class ["logo"]] [
                      div ~a:[a_class ["logo_image"]] [pcdata "put logo here"];
                      div ~a:[a_class ["site_name"]] [pcdata "Online board games"]
                    ];
              ] ();
            div ~a:[a_class ["header_login"]] [login]
          ];
      menu
    ] in
  let%lwt message_eref = Eliom_reference.get Common.message_next_page in
  let contents = match message_eref with
      None -> contents
    | Some message ->
       begin
         Lwt.ignore_result (Eliom_reference.set Common.message_next_page None);
         contents @ [br();
                     div ~a:[a_class ["user_message"]] [pcdata message]
                    ]
       end
  in
  Lwt.return (
      div contents
    )

let skeleton ?css:(css=[["css"; "TicTacToe.css"]]) ~title content =
  let%lwt header =
    header
      () in
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
