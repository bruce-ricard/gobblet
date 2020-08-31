open Ttt_common_lib_types

let dispatch game id =
  match game with
  | None ->
     begin
       Logs.warn (fun m -> m "Game dispatch: non existant game");
       Common.set_message_next_page
         "This game doesn't exist, please chose another one";
       Eliom_service.preapply Services.show_my_games_service ()
     end
  | Some game ->
     begin
       match game with
       | `TicTacToeClassical _ ->
          Eliom_service.preapply Services.ttt_classical_service id
       | `TicTacToeXOnly _ ->
          Eliom_service.preapply Services.ttt_xonly_service id
       | `ThreeMenMorris _ ->
          Eliom_service.preapply Services.ttt_3morris_service id
     end

let register () =
  Eliom_registration.Redirection.register
    ~service:Services.game_dispatch_service
    (fun id () ->
      let game = Common.Games.get_game (new id id) in
      Lwt.return (dispatch game id)
    )

let () = register ()
