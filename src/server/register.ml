open Eliom_lib
open Eliom_content.Html.D
open Services

let register_form () =
  Form.post_form
    ~service:user_registration_service
    (fun (name, (pwd1, pwd2)) ->
      [
        div
          [
            pcdata "Fill in the form to register.";
            br ();
            pcdata "Username: ";
            Form.input ~input_type:`Text ~name:name Form.string;
            br ();
            pcdata "Password: ";
            Form.input ~input_type:`Password ~name:pwd1 Form.string;
            br ();
            pcdata "Retype password: ";
            Form.input ~input_type:`Password ~name:pwd2 Form.string;
            br ();
            Form.input ~input_type:`Submit ~value:"Register" Form.string
          ]
      ]
    )
    ()

let user_registration_page () =
  Tttbase.skeleton ~title:"Register!" [register_form ()]
