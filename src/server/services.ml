let main_service =
  Eliom_service.create
    ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get (Eliom_parameter.unit))
    (*~https:true*)
    ()

let show_my_games_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["games"; "list"])
    ~meth:(Eliom_service.Get (Eliom_parameter.unit))
    ()

let game_dispatch_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["games"; "play"])
    ~meth:(Eliom_service.Get (Eliom_parameter.int "game_id"))
    ()

let ttt_classical_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["games"; "tictactoe"; "classical"; "play"])
    ~meth:(Eliom_service.Get (Eliom_parameter.int "game_id"))
    ()

let ttt_xonly_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["games"; "tictactoe"; "xonly"; "play"])
    ~meth:(Eliom_service.Get (Eliom_parameter.int "game_id"))
    ()

let ttt_3morris_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["games"; "3morris"; "play"])
    ~meth:(Eliom_service.Get (Eliom_parameter.int "game_id"))
    ()

let image_piece_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["games"; "pieces.png"])
    ~meth:(Eliom_service.Get (Eliom_parameter.unit))
    ()

let input_user_registration_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["register"])
    ~meth:(Eliom_service.Get (Eliom_parameter.unit))
    ()

let user_registration_service =
  Eliom_service.create
    (*    ~fallback:input_user_registration_service*)
    ~path:Eliom_service.No_path
    ~meth:(Eliom_service.Post
             (Eliom_parameter.unit,
              Eliom_parameter.
              (string "user_name" **
                 (string "password1" ** string "password2"))
             )
    )
    ()

let connection_service =
  Eliom_service.create
    ~path:Eliom_service.No_path
    ~meth:(Eliom_service.Post (
               Eliom_parameter.unit,
               Eliom_parameter.(string "name" ** string "password")
             )
    )

    ()

let disconnection_service =
  Eliom_service.create
    ~path:Eliom_service.No_path
    ~meth:(Eliom_service.Post (
               Eliom_parameter.unit,
               Eliom_parameter.unit
             )
    )
    ()
