let dbh = PGOCaml.connect ()

let put username password_hash =
  PGSQL(dbh) "insert into users values ($username, $password_hash)"

type check_response =
  | InvalidUser
  | CorrectPassword
  | WrongPassword

let get username (password_hash: string) =
  match
    PGSQL(dbh) "
              select password_hash
              from users
              where id=$username
                "
  with
    [] -> InvalidUser
  | [pwh] -> if pwh = password_hash then
               CorrectPassword
             else
               WrongPassword
  | _ -> failwith (Printf.sprintf
                     "Multiple users for user \"%s\""
                     username)

let exists username =
  let users = PGSQL(dbh) "
                          select 1 from users
                          where id=$username
                          " in
  match users with
  | [] -> false
  | [_] -> true
  | _ -> assert false
