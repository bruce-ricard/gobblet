open Eliom_content.Html.D

let _ = let open Common in let open Connection_code in ()

module TicTacToe_app =
  Eliom_registration.App (
      struct
        let application_name = "TicTacToe"
        let global_data_path = None
      end)

let header_login () =
  let%lwt current_user =
    Eliom_reference.get Common.current_user in
  match current_user with
  | Some (name, _) -> Lwt.return (
                     div
                       [
                         pcdata ("Logged in as " ^
                                   (String.capitalize name));
                         Connection_code.disconnect_box ()
                       ]
                   )
  | None -> Connection_code.connection_box ()

let%client remove_message_in_5 dom =
  let open Lwt in
  Lwt.async (fun () ->
      Lwt_js.sleep 5. >|=
        (fun () -> dom##.innerHTML := Js.string "")
    )

let%client show_instant_message event message_element =
  let message_dom =
    Eliom_content.Html5.To_dom.of_element message_element in
  remove_message_in_5 message_dom;
  React.E.map
    (fun msg -> message_dom##.innerHTML := Js.string msg;
                remove_message_in_5 message_dom;
    )
    event

let set_instant_message_ref () =
  Eliom_reference.set
    Common.instant_message_ref
    (Some (React.E.create ()))

let init_instant_message_ref () =
  let%lwt ref = Eliom_reference.get Common.instant_message_ref in
  match ref with
  | None -> set_instant_message_ref ()
  | Some _ -> Lwt.return ()

let header () =
  let%lwt () = init_instant_message_ref () in
  let%lwt login = header_login () in
  let menu =
    div ~a:[a_class ["menu"]]
        [
          a Services.show_my_games_service [pcdata "Play"] ()
        ]
  in
  let%lwt message_eref =
    Eliom_reference.get Common.message_next_page in
  let message = match message_eref with
      None -> []
    | Some message ->
       begin
         Lwt.ignore_result
           (Eliom_reference.set Common.message_next_page None);
         [div ~a:[a_class ["user_message"]] [pcdata message]]
       end
  in
  let instant_message =
    div ~a:[a_class ["instant_message"]] message in

  let%lwt instant_message_ref =
    Eliom_reference.get Common.instant_message_ref in
  let instant_message_event =
    match instant_message_ref with
      None -> Logs.err (fun m -> m "None instant message ref");
              assert false
    | Some (event,_) -> event
  in
  let down_event =
    Eliom_react.Down.of_react instant_message_event in

  let _ = [%client (show_instant_message
                      ~%down_event
                      ~%instant_message
                    : unit React.event)] in

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
      menu;
      instant_message
    ] in
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

let send_instant_message msg =
  let%lwt ref = Eliom_reference.get Common.instant_message_ref in
  match ref with
    Some (_,send_msg) -> Lwt.return (send_msg msg)
  | None ->
     Logs.err (fun m -> m "Cannot send instant message, no reference");
     Lwt.return ()
