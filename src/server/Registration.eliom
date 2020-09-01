let set_message = Common.set_message_next_page

let successful_registration_msg = "You successfully registered, please log-in to start playing."
let user_already_exists_msg = "This user already exists, please select another one."
let invalid_username_msg = "The username you entered is invalid. "
                           ^ "Usernames must be between 3 and 30 characters, "
                           ^ "contain only letters, numbers, dashes (-) and "
                           ^ "underscores (_)."

let non_matching_passwords_msg = "The passwords don't match, please try again."

let register () =
  let open Register in

  Eliom_registration.Html.register
    ~service:Services.input_user_registration_service
    (fun () () -> user_registration_page ());

  Eliom_registration.Redirection.register
    ~service:Services.user_registration_service
    (*    ~options:`TemporaryRedirect*)
    (fun ()  (user_name, (p1, p2)) ->
      Lwt.return (
          Logs.debug (fun m -> m "Registration attempt");
          if p1 = p2 then
          let open Ttt_user_lib_types in
          match Common.Users.register user_name p1 with
            Success ->
            begin
              Logs.info (
                  fun m -> m "%s just registered" user_name);
              set_message successful_registration_msg;
              Services.main_service
            end
          | UserAlreadyExists ->
             begin
               Logs.info (
                   fun m -> m "Cannot register %s, user already exists"
                              user_name
                 );
               set_message user_already_exists_msg;
               Services.input_user_registration_service
             end
          | InvalidUsername ->
             begin
               Logs.info (
                   fun m -> m "Invalid username \"%s\""
                              user_name
                 );
               set_message invalid_username_msg;
               Services.input_user_registration_service
             end
          | Error e ->
             begin
               Logs.err (fun m -> m "Error while registering: %s" e);
               set_message ("Error:" ^ e);
               Services.input_user_registration_service
             end
        else
          begin
            Logs.debug (fun m -> m "Registration passwords don't match");
            set_message non_matching_passwords_msg;
            Services.input_user_registration_service
          end
        )
    )

let () = register ()
