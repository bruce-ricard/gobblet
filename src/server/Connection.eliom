[%%shared
    open Eliom_lib
    open Eliom_content.Html.D
]

let current_user = Common.current_user

let welcome_message user =
  Some ("You successfully logged in. Welcome " ^ user)

let register () =
  Eliom_registration.Action.register
    ~service:Services.connection_service
    (fun () (name, password) ->
      match Common.Users.log_in name password with
      | None ->
         begin
           Eliom_reference.set Common.message_next_page
                               (Some ("Invalid username or password."))
         end
      | Some user ->
         begin
           let name = user#normalized_name in
           Logs.info (fun m -> m "%s just logged in" name);
           let%lwt () = Eliom_reference.set Common.message_next_page
                                            (welcome_message name) in
           let%lwt () = Eliom_reference.set current_user (Some(name,user))
           in
           Logs.debug (fun m -> m "Done logging in");
           Lwt.return ()
         end
    );

  Eliom_registration.Action.register
    ~service:Services.disconnection_service
    (fun () () ->
      let%lwt user = Eliom_reference.get current_user in
      (match user with
        None -> Logs.err (fun m -> m "log out from not logged in user!")
      | Some (user,_) ->
         begin
           Lwt.async (fun () -> Eliom_reference.set current_user None);
           Logs.info (fun m -> m "%s just logged out" user);
         end
      ) ; Lwt.return ()
    )

let () = register ()
