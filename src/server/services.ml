let main_service =
  Eliom_service.Http.service
    ~path:[]
    ~get_params:Eliom_parameter.unit
    (*~https:true*)
    ()

let show_my_games_service =
  Eliom_service.App.service
    ~path:["games"; "list"]
    ~get_params:Eliom_parameter.unit
    ()

let game_dispatch_service =
  Eliom_service.App.service
    ~path:["games"; "play"]
    ~get_params:(Eliom_parameter.int "game_id")
    ()

let ttt_classical_service =
  Eliom_service.App.service
    ~path:["games"; "tictactoe"; "classical"; "play"]
    ~get_params:Eliom_parameter.(int "game_id")
    ()

let ttt_xonly_service =
  Eliom_service.App.service
    ~path:["games"; "tictactoe"; "xonly"; "play"]
    ~get_params:Eliom_parameter.(int "game_id")
    ()

let ttt_3morris_service =
  Eliom_service.App.service
    ~path:["games"; "3morris"; "play"]
    ~get_params:Eliom_parameter.(int "game_id")
    ()

let image_piece_service =
  Eliom_service.App.service
    ~path:["games"; "pieces.png"]
    ~get_params:Eliom_parameter.unit
    ()

let input_user_registration_service =
  Eliom_service.Http.service
    ~path:["register"]
    ~get_params:Eliom_parameter.unit
    ()

let user_registration_service =
  Eliom_service.Http.post_service
    ~fallback:input_user_registration_service
    ~post_params:Eliom_parameter.
      (string "user_name" **
         (string "password1" ** string "password2"))
    ()

let connection_service =
  Eliom_service.Http.post_coservice'
    ~post_params:Eliom_parameter.(string "name" ** string "password")
    ()

let disconnection_service =
  Eliom_service.Http.post_coservice'
    ~post_params:Eliom_parameter.unit
    ()
