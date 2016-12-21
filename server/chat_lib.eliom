open Eliom_content.Html5.D

let chat_logs_html () =
  let x, ux = React.E.create () in
  let r =  Eliom_react.Down.of_react x in

  let lir = li [pcdata "0"] in
  let html =
    ul [
        li [pcdata "test"];
        li [pcdata "123"];
        lir
      ]
  in
  let _ = [%client
              (
              (*                Lwt.async (fun () ->*) (
                (
                      let dom_cell = Eliom_content.Html5.To_dom.of_element ~%lir in
                      dom_cell##.innerHTML := Js.string "wazuuuuup";
                      let _ =
                        React.E.map
                          (fun i ->
                            dom_cell##.innerHTML := Js.string (string_of_int i)
                          )
                          ~%r
                      in
                      ()
                    (*                      Lwt.return ()*)
                    )
                )
                : unit
              )
          ]
  in
  let _ =
    Lwt.async (fun () ->
        let%lwt _ = Lwt_unix.sleep 3. in
        List.fold_left
             (fun y x -> Lwt.bind y (fun () -> ux x; Lwt_unix.sleep 0.3))
             (Lwt.return ())
             [1;2;3;4;5;6;7;8;9;10]

      ) in
  let pr = React.E.map print_int x in
  html

let%client chat_form_handler input_text_field submit_button =
  let dom_text = Eliom_content.Html5.To_dom.of_input input_text_field in
  let dom_button = Eliom_content.Html5.To_dom.of_element submit_button in
  Lwt.async (fun () ->
      Lwt_js_events.clicks
        dom_button
        (fun _ _ ->
          let message = Js.to_string dom_text##.value in
          let new_chat_line =
            li [pcdata (Printf.sprintf "me: %s" message)] in
          Eliom_content.Html5.Manip.appendChild ~%chat_logs_elt new_chat_line;
          ignore (
              Eliom_client.call_service
                ~service:~%chat_service
                () (message_id, message)
            );
          dom_text##.value := Js.string "";
          Lwt.return ()
        )
    )

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
