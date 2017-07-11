type registration_result =
  | Success
  | UserAlreadyExists
  | InvalidUsername
  | Error of string
