let main_service =
  Eliom_service.Http.service
    ~path:[]
    ~get_params:Eliom_parameter.unit
    ~https:true
    ()

let show_my_games_service =
  Eliom_service.App.service
    ~path:["games"; "tictactoe"; "list"]
    ~get_params:Eliom_parameter.unit
    ()

let ttt_service =
  Eliom_service.App.service
    ~path:["games"; "tictactoe"; "play"]
    ~get_params:Eliom_parameter.(int "game_id")
    ()

let input_create_game_service =
  Eliom_service.Http.service
    ~path:["games"; "tictactoe"; "newgame"]
    ~get_params:Eliom_parameter.unit
    ()

let create_game_service =
  Eliom_service.Http.service
    ~path:["games"; "tictactoe"; "newgame"]
    ~get_params:Eliom_parameter.(string "oponent")
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

let chat_service =
  Eliom_service.App.post_coservice'
    ~post_params:Eliom_parameter.(string "message")
    ()
