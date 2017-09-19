[%%shared
 open Ttt_common_lib_types
 open Eliom_lib
 open Eliom_content.Html5.D
]

module TTTBasic = Common.Tic_tac_toe_classical

let current_user = Common.current_user

let create_challenge opponent =
  let%lwt user = Eliom_reference.get current_user in
  match user with
  | None -> Lwt.return `NotLoggedIn
  | Some (user,_) ->
     begin
       let challenge =
         if opponent = "" then
           TTTBasic.new_challenge user
         else
           TTTBasic.new_challenge user ~opponent
       in
       let open Ttt_server_lib_types in
       match challenge with
       | Challenge_created(id,event) ->
          Lwt.async (fun () ->
              Base.send_instant_message "Challenge created");
          Lwt.return (`ChallengeCreated
                       (id#get_id,Eliom_react.Down.of_react event)
                     )
       | Challenge_accepted id -> Lwt.return (`ChallengeAccepted id#get_id)
       | Error e -> Lwt.return (`Error e)
     end

let accept_challenge id_int =
  let%lwt user = Eliom_reference.get current_user in
  Lwt.return (match user with
              | None -> `NotLoggedIn
              | Some (user, _) ->
                 if TTTBasic.accept_challenge (new id id_int) user then
                   `Success
                 else
                   `Fail)

let%client create_challenge_rpc =
  ~%(server_function [%derive.json: string] create_challenge)

let%client accept_challenge_rpc =
  ~%(server_function [%derive.json: int] accept_challenge)

let create_challenge_form user =
  [div
     [
       pcdata "Play against: ";
       Form.input ~input_type:`Text ~name:user Form.string;
       Form.input ~input_type:`Submit ~value:"Challenge" Form.string
     ]
  ]

let%client challenge_form_handler input_text_field submit_button =
  let dom_text = Eliom_content.Html5.To_dom.of_input input_text_field
  and dom_button = Eliom_content.Html5.To_dom.of_element submit_button in
  Lwt.async (fun () ->
      Lwt_js_events.clicks
        dom_button
        (fun _ _ ->
          let opponent = Js.to_string dom_text##.value in
          let%lwt challenge = create_challenge_rpc opponent in
          (match challenge with
           | `NotLoggedIn -> Eliom_lib.alert "Log in to create a challenge."
           | `ChallengeCreated (id,event) ->
              ignore @@
                React.E.map (fun () -> Eliom_client.change_page
                                         ~%Services.ttt_service
                                         id
                                         ())
                            event

           | `ChallengeAccepted id ->
              ignore @@ Eliom_client.change_page
                          ~%Services.ttt_service
                          id
                          ()
           | `Error e -> Eliom_lib.alert "Error: %s"  e
          );
          Lwt.return ()
        )
    )

let challenge_form () =
  let button =
    Form.input ~input_type:`Submit ~value:"Challenge" Form.string
  and input_text_field =
    Form.input ~input_type:`Text  Form.string in
  let form =
    div [
        input_text_field;
        button
      ]
  in
  let _ = [%client
              (
                challenge_form_handler ~%input_text_field ~%button;
                () : unit
              )
          ] in
  form

let%client accept_challenge_handler accept_button id =
  let button_dom =
    Eliom_content.Html5.To_dom.of_element accept_button in
  Lwt.async (fun () ->
      Lwt_js_events.clicks
        button_dom
        (fun _ _ ->
          print_endline "accept button was clicked";
          let%lwt response = accept_challenge_rpc id in
          print_endline "got accept challenge response";
          match response with
          | `NotLoggedIn ->
             begin
               print_endline "Not logged player tried to accept challenge";
               Eliom_lib.alert "Log in to play!";
               Lwt.return ()
             end
          | `Fail ->
             print_endline "failed to accept challenge, too late";
             Lwt.return ()
          | `Success ->
             print_endline "challenge accepted, forwarding to game page";
             Eliom_client.change_page
               ~%Services.ttt_service
               id
               ()
        )
    )

let%shared challenge_html ((id : int), challenger) =
  let accept_button =
    Form.input ~input_type:`Submit ~value:"Accept" Form.string in
  let element =
    div [
        pcdata challenger;
        accept_button
      ]
  in
  let _ = [%client
              (accept_challenge_handler ~%accept_button ~%id : unit)
          ] in
  element

let%shared challenges_to_html challenges =
  List.map challenge_html challenges

let challenge_elements user =
  let private_challenges = TTTBasic.get_private_challenges user
  and public_challenges = TTTBasic.get_public_challenges user in
  let open Ttt_server_lib_types in
  let private_challenges_elt =
    div (challenges_to_html private_challenges.initial_data)
  and public_challenges_elt =
    div (challenges_to_html public_challenges.initial_data) in
  let element =
    div [
        h3 [pcdata "Your private challenges:"];
        private_challenges_elt;
        h3 [pcdata "Public challenges:"];
        public_challenges_elt
      ]
  in
  let public_event = Eliom_react.Down.of_react public_challenges.event
  and private_event = Eliom_react.Down.of_react  private_challenges.event
  in
  let _ = [%client
              (ignore @@ React.E.map (fun l ->
                             Eliom_content.Html5.Manip.replaceChildren
                               ~%public_challenges_elt
                               (challenges_to_html l))
                                     ~%public_event;

               ignore @@ React.E.map (fun l ->
                             Eliom_content.Html5.Manip.replaceChildren
                               ~%private_challenges_elt
                               (challenges_to_html l))
                                     ~%private_event;
               : unit)
          ]
  in
  element

let show_my_games_page () =
  let%lwt current_user = Eliom_reference.get current_user in
  let game_list =
    match current_user with
      None -> div [pcdata "Log in to save your games"]
    | Some (user, _) ->
       let idgame_to_link (id, opp) =
         a Services.ttt_service [pcdata opp] id#get_id in
       let games = TTTBasic.get_current_games user in
       let games_display =
         match games with
         | [] -> [pcdata "You have no games in progress, create a challenge to play."]
         | games ->
            let links = List.map idgame_to_link games in
            let bullets = List.map (fun link -> li [link]) links in
            [ul bullets]
       in
       div (challenge_form () :: (challenge_elements user) :: games_display)
  in
  Base.skeleton
    ~css:[["css"; "TicTacToe.css"]]
    ~title:"Game list"
    [game_list]

let eliom_register () =
  let open Services in

  Eliom_registration.Action.register
    ~service:Services.create_challenge_service
    (fun opponent () ->
      match create_challenge opponent with
      | _ ->
         begin
           Logs.debug (fun m -> m "Challenge request received");
           Lwt.return () (* TODO do stuff here*)
         end
    );

  Base.TicTacToe_app.register
    ~service:show_my_games_service
    (fun () () -> show_my_games_page ())

let () = eliom_register ()
