[%%shared
 open Eliom_content.Html.D
]
let chat_logs_elt =
  ul [
    ]

(*

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
                ~service:~%Services.chat_service
                () message
            );
          dom_text##.value := Js.string "";
          Lwt.return ()
        )
    )

let chat_input () =
  let submit_button =
    Form.input ~input_type:`Submit ~value:"Send" Form.string in
  let input_text_field = Form.input ~input_type:`Text  Form.string in
  let form =
    div [
        input_text_field;
        submit_button
      ]
  in
  let _ = [%client
              (
                chat_form_handler ~%input_text_field ~%submit_button;
                () : unit
              )
          ] in
  form

let chat_input_text_html () =
  div []

let chat_logs_html () = div []

let chat_html () =
  let elt =
    div [
        h3 [pcdata "Chat window"];
        chat_logs_html ();
        chat_input_text_html ()
      ] in
  elt
 *)
