open PostgresConfiguration

module Make(Config : CONFIG) =
  struct

    let log_config () =
      let ots f = function
          None -> "None"
        | Some s -> f s
      in
      let sts = ots (fun x -> x)
      and its = ots string_of_int in
      let open Config in
      let host = sts host
      and port = its port
      and user = sts user
      and db = sts database in
      Logs.info (fun m ->
          m "Host: %s, port: %s, user: %s, database: %s"
            host
            port
            user
            db
        )

    let dbh =
      let open Config in
      log_config ();
      PGOCaml.connect
        ?host
        ?port
        ?user
        ?password
        ?database
        ()

    type rating = Ttt_common_lib_types.rating

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

    let data_to_rating (rating, rating_deviation, sigma) =
      let open Ttt_common_lib_types in
      {rating; rating_deviation; sigma}

    let request_result_to_rating = function
      | [] -> None
      | [r,rd,s] ->
         let fos = float_of_string in
         Some (data_to_rating (fos r, (fos rd), (fos s)))
      | _ -> assert false

    let get_tictactoeclassical_rating username =
      Logs.debug (fun m -> m "Getting rating for user %s" username);
      request_result_to_rating (
          PGSQL(dbh)
            "
             SELECT rating, rd, sigma
             FROM ratings.tictactoeclassical
             WHERE username=$username
             "
        )

    let get_tictactoexonly_rating username =
      Logs.debug (fun m -> m "Getting rating for user %s" username);
      request_result_to_rating (
          PGSQL(dbh)
            "
             SELECT rating, rd, sigma
             FROM ratings.tictactoexonly
             WHERE username=$username
             "
        )

    let get_3menmorris_rating username =
      Logs.err (fun m -> m "Not getting 3 men morris rating, not implemented yet");
      (*Some(data_to_rating (100.,1.,0.01))*)
      None

    let get_rating = function
      | `TicTacToeClassical -> get_tictactoeclassical_rating
      | `TicTacToeXOnly -> get_tictactoexonly_rating
      | `ThreeMenMorris -> get_3menmorris_rating


    let upsert_ttt_classical username rating rating_deviation sigma =
      ignore (PGSQL(dbh)
                "
                 INSERT INTO ratings.tictactoeclassical
                 VALUES ($username, $rating, $rating_deviation, $sigma)
                 ON CONFLICT (username) DO UPDATE
                 SET rating = $rating,
                 rd = $rating_deviation,
                 sigma = $sigma
                 "
        );
      true

    let upsert_ttt_xonly username rating rating_deviation sigma =
      ignore (PGSQL(dbh)
                "
                 INSERT INTO ratings.tictactoexonly
                 VALUES ($username, $rating, $rating_deviation, $sigma)
                 ON CONFLICT (username) DO UPDATE
                 SET rating = $rating,
                 rd = $rating_deviation,
                 sigma = $sigma
                 "
        );
      true

    let upsert_3_men_morris username rating rating_deviation sigma =
      Logs.err (fun m -> m "Not setting 3 men morris rating, not implemented yet");
      true

    let set_rating
          game
          username =
      let open Ttt_common_lib_types in
      function {rating; rating_deviation; sigma} ->
        Logs.debug (fun m -> m "Setting rating for user %s" username);
        let rating = string_of_float rating
        and rating_deviation = string_of_float rating_deviation
        and sigma = string_of_float sigma in
        let upsert = match game with
          | `TicTacToeClassical -> upsert_ttt_classical
          | `TicTacToeXOnly -> upsert_ttt_xonly
          | `ThreeMenMorris -> upsert_3_men_morris
        in
        upsert username rating rating_deviation sigma
  end
