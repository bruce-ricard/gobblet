[%%shared
 open Ttt_common_lib_types
 open Eliom_lib
 open Eliom_content.Html5.D
]

module TTTBasic = Common.TicTacToeClassical
module Games = Common.Games

let current_user = Common.current_user

let create_challenge game opponent =
  let%lwt user = Eliom_reference.get current_user in
  match user with
  | None -> Lwt.return `NotLoggedIn
  | Some (user,_) ->
     begin
       let challenge =
         if opponent = "" then
           Games.new_challenge user game
         else
           Games.new_challenge user ~opponent game
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

let parse_game_id = function
  | 0 -> None
  | 1 -> Some(`TicTacToeClassical)
  | 2 -> Some(`TicTacToeXOnly)
  | n -> Logs.warn (fun m -> m "Illegal game_id %d" n); None

let create_challenge_by_id (game_id, opp) =
  let int_id =
    try
      Some (int_of_string game_id)
    with
      _ -> Logs.err (fun m -> m "Invalid game id %s" game_id); None
  in
  match int_id with
  | Some id ->
     let game = parse_game_id id in
     create_challenge game (String.lowercase opp)
  | None -> Lwt.return (`Error "Invalid game ID")

let accept_challenge id_int =
  let%lwt user = Eliom_reference.get current_user in
  Lwt.return (match user with
              | None -> `NotLoggedIn
              | Some (user, _) ->
                 if Games.accept_challenge (new id id_int) user then
                   `Success
                 else
                   `Fail)

let%client create_challenge_rpc =
  ~%(server_function [%derive.json: string * string]
                     (create_challenge_by_id)
    )

let%client accept_challenge_rpc =
  ~%(server_function [%derive.json: int] accept_challenge)

let%client challenge_form_handler
           input_text_field game_name_field submit_button =
  let dom_text = Eliom_content.Html5.To_dom.of_input input_text_field
  and dom_button = Eliom_content.Html5.To_dom.of_element submit_button
  and dom_game_name = Eliom_content.Html5.To_dom.of_select game_name_field
  in
  Lwt.async (fun () ->
      Lwt_js_events.clicks
        dom_button
        (fun _ _ ->
          let opponent = Js.to_string dom_text##.value in
          let game_name_int =
            Js.to_string
              dom_game_name##.value in
          print_endline game_name_int;
          let%lwt challenge = create_challenge_rpc (game_name_int, opponent) in
          begin
            match challenge with
            | `NotLoggedIn -> Eliom_lib.alert "Log in to create a challenge."
            | `ChallengeCreated (id,event) ->
               ignore @@
                 React.E.map (fun () -> Eliom_client.change_page
                                          ~%Services.game_dispatch_service
                                          id
                                          ())
                             event

            | `ChallengeAccepted id ->
               ignore @@ Eliom_client.change_page
                           ~%Services.game_dispatch_service
                           id
                           ()
            | `Error e -> Eliom_lib.alert "Error: %s" e
          end;
          Lwt.return ()
        )
    )

let challenge_form () =
  let button =
    Form.input ~input_type:`Submit ~value:"Challenge" Form.string
  and input_text_field =
    Form.input ~input_type:`Text Form.string
  and game_name_select =
    Raw.select [
        option ~a:[a_value "0"] (pcdata "Any");
        option ~a:[a_value "1"; a_selected `Selected]
               (pcdata "Tic tac toe -- classical");
        option ~a:[a_value "2"]
               (pcdata "Tic tac toe -- X only");
      ]
  in
  let form =
    div [
        input_text_field;
        game_name_select;
        button
      ]
  in
  let _ = [%client
              (
                challenge_form_handler
                  ~%input_text_field ~%game_name_select ~%button;
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
               ~%Services.game_dispatch_service
               id
               ()
        )
    )

let%client game_name_to_string = function
  | None -> "random"
  | Some game ->
     begin
       match game with
       | `TicTacToeClassical -> "Tic Tac Toe classical"
       | `TicTacToeXOnly -> "Tic Tac Toe X only"
       | `ThreeMenMorris -> "Three men morris"
     end

let%client challenge_html challenge =
  let accept_button =
    Form.input ~input_type:`Submit ~value:"Accept" Form.string in
  let element =
    tr
      [
        td [pcdata (game_name_to_string challenge.game_type)];
        td [pcdata challenge.challenger];
        td [accept_button]
      ]
  in
  accept_challenge_handler accept_button challenge.id;
  element

let%client challenges_to_html challenges =
  table (List.map challenge_html challenges)

let challenge_elements user =
  let private_challenges = Games.get_private_challenges user
  and public_challenges = Games.get_public_challenges user in
  let open Ttt_server_lib_types in
  let private_challenges_elt =
    div []
  and public_challenges_elt =
    div []
  in
  let element =
    div [
        h3 [pcdata "Your private challenges:"];
        private_challenges_elt;
        h3 [pcdata "Public challenges:"];
        public_challenges_elt
      ]
  in
  let public_event = Eliom_react.Down.of_react public_challenges
  and private_event = Eliom_react.Down.of_react private_challenges
  in
  let _ = [%client
              (ignore @@ React.E.map (fun l ->
                             Eliom_content.Html5.Manip.replaceChildren
                               ~%public_challenges_elt
                               [(challenges_to_html l)])
                                     ~%public_event;

               ignore @@ React.E.map (fun l ->
                             Eliom_content.Html5.Manip.replaceChildren
                               ~%private_challenges_elt
                               [(challenges_to_html l)])
                                     ~%private_event;
               : unit)
          ]
  in
  element

let game_list_page () =
  let%lwt current_user = Eliom_reference.get current_user in
  let game_list =
    match current_user with
      None -> div [pcdata "Log in to save your games"]
    | Some (user, _) ->
       let idgame_to_link (id, opp) =
         a Services.game_dispatch_service [pcdata opp] id#get_id in
       let games = Games.get_current_games user in
       let games_display =
         match games with
         | [] -> [pcdata "You have no games in progress, create a challenge to play."]
         | games ->
            let links = List.map idgame_to_link games in
            let bullets = List.map (fun link -> li [link]) links in
            [ul bullets]
       in
       div (games_display @ challenge_form () :: (challenge_elements user) ::
                              [])
  in
  Base.skeleton
    ~css:[["css"; "TicTacToe.css"]]
    ~title:"Game list"
    [game_list]

let eliom_register () =
  let open Services in

  Base.TicTacToe_app.register
    ~service:show_my_games_service
    (fun () () -> game_list_page ())

let () = eliom_register ()
