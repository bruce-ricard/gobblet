let current_user =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    (None : (string * User.user) option)

let message_next_page =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    (None : string option)

let set_message m : unit =
  Lwt.async (fun () -> Eliom_reference.set message_next_page (Some m))

            (* difference between Lwt.async and ignore?*)
