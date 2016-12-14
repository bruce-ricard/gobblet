open Eliom_content.Html5.D

let header_login connection_box disconnect_box () =
  let%lwt current_user = Eliom_reference.get Common.current_user in
  match current_user with
  | Some (name, _) -> Lwt.return (
                     div
                       [
                         pcdata ("Logged in as " ^ name);
                         disconnect_box ()
                       ]
                   )
  | None -> connection_box ()

let header connection_box disconnect_box main_service show_games () =
  let%lwt login = header_login connection_box disconnect_box () in
  let menu =
    div ~a:[a_class ["menu"]]
        [
          a show_games [pcdata "Tic Tac Toe"] ()
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

let skeleton ?css:(css=[["css"; "TicTacToe.css"]]) ~title content =
  let%lwt header =
    header
    Connection_code.connection_box
    Connection_code.disconnect_box
    Services.main_service
    Services.show_my_games_service
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
