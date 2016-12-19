let current_user =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    (None : (string * User.user) option)

let message_next_page =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    (None : string option)
