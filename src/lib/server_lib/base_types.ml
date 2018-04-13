type challenge = Ttt_server_lib_challenge.t

type remove_challenge =
  | Id_not_present
  | Deleted of challenge
