let () =
  Initializer.init ()

module Games = Module_instanciation.Games
module Users =  Module_instanciation.Users

module GameList =  Module_instanciation.GameList

module TicTacToeClassical =
  GameList.TicTacToeClassical

module TicTacToeXOnly =
  GameList.TicTacToeXOnly

module ThreeMenMorris =
  GameList.ThreeMenMorris

let current_user =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    (None : (string * Ttt_user_lib_user.user) option)

let message_next_page =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    (None : string option)

let set_message_next_page m : unit =
  Lwt.async (fun () -> Eliom_reference.set message_next_page (Some m))

let instant_message_ref =
  Eliom_reference.eref
    ~scope:Eliom_common.default_process_scope
    (None :
       (string React.event * ((?step:React.step -> string -> unit))) option
    )
