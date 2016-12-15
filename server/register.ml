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
  ()

let user_registration_page () =
  Base.skeleton ~title:"Register!" [register_form ()]
