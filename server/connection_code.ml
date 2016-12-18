open Eliom_content.Html5.D

let _ = let open Common in ()

let connection_box () =
  let%lwt user = Eliom_reference.get Common.current_user in
    Lwt.return
      (match user with
       | Some (user,_) -> p [pcdata "You are connected as "; pcdata user]
       | None ->
          Form.post_form ~service:Services.connection_service
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
                                 Form.string;
                               pcdata "or ";
                               a
                                 ~service:Services.input_user_registration_service
                                 [pcdata "register."]
                                 ()
                         ]])
                         ()
      )

let disconnect_box () =
  Form.post_form
    ~service:Services.disconnection_service
    (
      fun () ->
      [Form.input ~input_type:`Submit ~value:"Log out" Form.string]
    )
    ()
