let register () =
  let open Register in

  Eliom_registration.Html5.register
    ~service:Services.input_user_registration_service
    (fun () () -> user_registration_page ());

  Eliom_registration.Redirection.register
    ~service:Services.user_registration_service
    ~options:`TemporaryRedirect
    (fun () (user_name, (p1, p2)) ->
      if p1 = p2 then
        Users.register user_name p1
      else
        failwith "non matching passwords"
     ;
       Lwt.return Services.main_service )

let () = register ()
