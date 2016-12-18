let register () =
  let open Register in

  Eliom_registration.Html5.register
    ~service:Services.input_user_registration_service
    (fun () () -> user_registration_page ());

  Eliom_registration.Redirection.register
    ~service:Services.user_registration_service
    (*    ~options:`TemporaryRedirect*)
    (fun ()  (user_name, (p1, p2)) ->
      print_endline "wtf I'm here";
      let _ = [%client (Eliom_lib.alert "I'm here" : unit)] in
      begin
      try
      if p1 = p2 then
        let open Users in
        match register user_name p1 with
          Success -> let _ = [%client (let () = Eliom_lib.alert "success registered!" in (): unit)] in ()
        | UserAlreadyExists -> let _ = [%client (let () = Eliom_lib.alert "UAE" in () : unit)] in ()
        | Error e -> let _ = [%client (let () = Eliom_lib.alert ("Error: ") in () : unit)] in ()
      else
        failwith "non matching passwords"
      with
        e -> let _ = [%client (let () = Eliom_lib.alert ("Error: ") in () : unit)] in ()
        end;
       let _ = [%client
                (let _ = Eliom_lib.alert "plop" in () : unit)
               ] in
       Lwt.return Services.main_service )

let () = register ()
