let dbh = PGOCaml.connect
            (*~host:"localhost"
            ~port:5433
            ~user:"ubuntu"
            (*            ~password:""*)
            ~database:"ubuntu" *)
            ()

let put_hash username password_hash =
  PGSQL(dbh) "insert into users values ($username, $password_hash)";
  true

let (put : string -> Sha256.t -> bool) username password_hash  =
  put_hash username (Sha256.to_hex password_hash)

let get username (password_hash: Sha256.t) =
  let password_hash = Sha256.to_hex password_hash in
  match
    PGSQL(dbh) "
              select password_hash
              from users
              where id=$username
                "
  with
    [] -> false
  | [pwh] -> pwh = password_hash
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
