let set_message = Common.set_message

let successful_registration_msg = "You successfully registered, please log-in to start playing."
let user_already_exists_msg = "This user already exists, please select another one."
let non_matching_passwords_msg = "The passwords don't match, please try again."

let register () =
  let open Register in

  Eliom_registration.Html5.register
    ~service:Services.input_user_registration_service
    (fun () () -> user_registration_page ());

  Eliom_registration.Redirection.register
    ~service:Services.user_registration_service
    (*    ~options:`TemporaryRedirect*)
    (fun ()  (user_name, (p1, p2)) ->
      Lwt.return (
        if p1 = p2 then
          let open Types in
          match Users.register user_name p1 with
            Success ->
            begin
              set_message successful_registration_msg;
              Services.main_service
            end
          | UserAlreadyExists ->
             begin
               set_message user_already_exists_msg;
               Services.input_user_registration_service
               end
          | Error e ->
             begin
               set_message ("Error:" ^ e);
               Services.input_user_registration_service
             end
        else
          begin
            set_message non_matching_passwords_msg;
            Services.input_user_registration_service
          end
        )
    )

let () = register ()
