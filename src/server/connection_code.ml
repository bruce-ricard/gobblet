open Eliom_content.Html.D

let _ = let open Common in ()

let connection_form_fun (name, pwd) =
  [fieldset
     [label [pcdata "login: "];
      Form.input
        ~input_type:`Text ~name:name
        Form.string;
      br ();
      label [pcdata "password: "];
      Form.input
        ~input_type:`Password ~name:pwd
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
     ]
  ]

let connection_box () =
  Lwt.return
    (Form.post_form
       ~service:Services.connection_service
       connection_form_fun
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
