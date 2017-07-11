let dbh = PGOCaml.connect ()

let read () =
  PGSQL(dbh) "select * from users"

let rec users_to_string = function
    [] -> ""
  | (id, pwd, Some email) :: us -> Printf.sprintf "<%s, %s>\n" id email ^
                                     (users_to_string us)
  | (id, pwd, None) :: us -> Printf.sprintf "<%s, %s>\n" id "-" ^
                                (users_to_string us)

let () =
  ignore (PostgresDao.put "b3" (Sha256.string "dada"));
  print_endline (users_to_string (read ()))
