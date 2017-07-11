[%%shared
    open Eliom_lib
    open Eliom_content.Html5.D
]

let current_user = Common.current_user

let register () =
  Eliom_registration.Action.register
    ~service:Services.connection_service
    (fun () (name, password) ->
      match Common.Users.log_in name password with
        None -> Lwt.return ()
      | Some user ->
           Eliom_reference.set current_user (Some(name,user))
    );

  Eliom_registration.Action.register
    ~service:Services.disconnection_service
    (fun () () ->
      (Eliom_reference.set current_user (None));
    )

let () = register ()
