let dbh = PGOCaml.connect
            (*~host:"localhost"
            ~port:5433
            ~user:"ubuntu"
            (*            ~password:""*)
            ~database:"ubuntu" *)
            ()

let get_rating username =
  match [%pgsql dbh "
                    SELECT rating, rd, sigma
                    FROM ratings.tictactoeclassical
                    WHERE username=$username
                     "
        ]
  with
    [] -> (1500., 350., 0.06)
  | [r,rd,s] ->
     let fos = float_of_string in
     fos r, (fos rd), (fos s)
  | _ -> assert false

let update_rating username rating deviation sigma =
  ()
