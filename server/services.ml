let main_service =
  Eliom_service.Http.service
    ~path:[]
    ~get_params:Eliom_parameter.unit
    (*    ~https:true*)
    ()

let show_my_games_service =
  Eliom_service.Http.service
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
