open Eliom_lib
open Eliom_content.Html5.D
open Services

module Users = Users.Users_test

let register_form () =
  Form.post_form
    ~service:user_registration_service
    (fun (name, (pwd1, pwd2)) ->
      [
        div
          [
            pcdata "Fill in the form to register.";
            Form.input ~input_type:`Text ~name:name Form.string;
            Form.input ~input_type:`Password ~name:pwd1 Form.string;
            Form.input ~input_type:`Password ~name:pwd2 Form.string;
            Form.input ~input_type:`Submit ~value:"Register" Form.string
          ]
      ]
    )

let user_registration_page () =
  Base.skeleton ~title:"Register!" [(div [])]

let register () =
  Eliom_registration.Html5.register
    ~service:input_user_registration_service
    (fun () () -> user_registration_page ());

  Eliom_registration.Redirection.register
    ~service:user_registration_service
    ~options:`TemporaryRedirect
    (fun () (user_name, (p1, p2)) ->
      if p1 = p2 then
        Users.register user_name p1
      else
        failwith "non matching passwords"
     ;
       Lwt.return Services.main_service )
