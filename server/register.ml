open Eliom_lib
open Eliom_content.Html5.D

let register_form ~service =
  Form.post_form
    ~service
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
