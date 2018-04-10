open PostgresConfiguration

module Make(Config : CONFIG) =
  struct

    let log_config () =
      let option_to_string f = function
          None -> "None"
        | Some s -> f s
      in
      let ots = option_to_string in
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
      try
        PGSQL(dbh) "insert into users values ($username, $password_hash)";
        true
      with
        e -> Logs.err (fun m ->
                 m "Exception while inserting new user in postgres.\n %s"
                   (Printexc.to_string e)
               ); false


    let (put : string -> Sha256.t -> bool) username password_hash  =
      put_hash username (Sha256.to_hex password_hash)

    let get username (password_hash: Sha256.t) =
      let password_hash = Sha256.to_hex password_hash in
      try
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
      with
        e -> Logs.err (fun m ->
                 m "Exception while getting user from postgres.\n %s"
                   (Printexc.to_string e)
               ); false


    let exists username =
      try
        let users =
          PGSQL(dbh)
               "
                select 1 from users
                where id=$username
                "
        in
        match users with
        | [] -> false
        | [_] -> true
        | _ -> assert false
      with
        e -> Logs.err (fun m ->
                 m "Exception while checking if user exists in postgres.\n %s"
                   (Printexc.to_string e)
               ); false

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
          try
            PGSQL(dbh)
                 "
                  SELECT rating, rd, sigma
                  FROM ratings.tictactoeclassical
                  WHERE username=$username
                  "
          with
            e -> Logs.err (fun m ->
                 m "Exception while getting tttc rating from postgres.\n %s"
                   (Printexc.to_string e)
               ); []
        )

    let get_tictactoexonly_rating username =
      Logs.debug (fun m -> m "Getting rating for user %s" username);
      request_result_to_rating (
          try
            PGSQL(dbh)
                 "
                  SELECT rating, rd, sigma
                  FROM ratings.tictactoexonly
                  WHERE username=$username
                  "
          with
            e -> Logs.err (fun m ->
                 m "Exception while getting tttxo rating from postgres.\n %s"
                   (Printexc.to_string e)
               ); []

        )

    let get_3menmorris_rating username =
      Logs.debug (fun m -> m "Getting rating for user %s" username);
      request_result_to_rating (
          try
            PGSQL(dbh)
                 "
                  SELECT rating, rd, sigma
                  FROM ratings.three_men_morris
                  WHERE username=$username
                  "
          with
            e -> Logs.err (fun m ->
                     m "%s %s\n %s"
                       "Exception while getting "
                       "three mm rating from postgres."
                       (Printexc.to_string e)
                   ); []
        )

    let get_rating = function
      | `TicTacToeClassical -> get_tictactoeclassical_rating
      | `TicTacToeXOnly -> get_tictactoexonly_rating
      | `ThreeMenMorris -> get_3menmorris_rating


    let upsert_ttt_classical username rating rating_deviation sigma =
      try
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
      with
        e -> Logs.err (fun m ->
                 m "Exception while updating tttc rating from postgres.\n %s"
                   (Printexc.to_string e)
               ); false


    let upsert_ttt_xonly username rating rating_deviation sigma =
      try
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
      with
        e -> Logs.err (fun m ->
                 m "Exception while updating tttxo rating from postgres.\n %s"
                   (Printexc.to_string e)
               ); false


    let upsert_3_men_morris username rating rating_deviation sigma =
      try
        ignore (PGSQL(dbh)
                  "
                   INSERT INTO ratings.three_men_morris
                   VALUES ($username, $rating, $rating_deviation, $sigma)
                   ON CONFLICT (username) DO UPDATE
                   SET rating = $rating,
                   rd = $rating_deviation,
                   sigma = $sigma
                   "
          );
        true
      with
        e -> Logs.err (fun m ->
                 m "Exception while inserting in postgres.\n %s"
                   (Printexc.to_string e)
               ); false

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
