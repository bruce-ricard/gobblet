[%%shared
    open Eliom_lib
    open Eliom_content.Html5.D
           (*    open Lwt*)

    module TTT = User.TTT
    module TTTUsers = Users.Users_test
]

let current_user = Base.current_user

let connection_service =
  Eliom_service.Http.post_coservice'
    ~post_params:Eliom_parameter.(string "name" ** string "password")
    ()

let disconnection_service =
  Eliom_service.Http.post_coservice'
    ~post_params:Eliom_parameter.unit
    ()

let connection_box () =
  let%lwt user = Eliom_reference.get current_user in
    Lwt.return
      (match user with
       | Some (user,_) -> p [pcdata "You are connected as "; pcdata user]
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

let disconnect_box () =
  Form.post_form
    ~service:disconnection_service
    (
      fun () ->
      [Form.input ~input_type:`Submit ~value:"Log out" Form.string]
    )
    ()

let register () =
  Eliom_registration.Action.register
    ~service:connection_service
    (fun () (name, password) ->
      match TTTUsers.log_in name password with
        None -> Lwt.return ()
      | Some user -> Eliom_reference.set current_user (Some(name,user))
    );

  Eliom_registration.Action.register
    ~service:disconnection_service
    (fun () () ->
      (Eliom_reference.set current_user (None));
    )

let () = register ()
