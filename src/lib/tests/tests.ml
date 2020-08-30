let () =
  Lwt_main.run
  @@ Alcotest_lwt.run
       "game lib"
       [
         "board suite",
         (List.map
            (fun (name, speed, test) ->
              Alcotest_lwt.test_case_sync
                name
                speed
                (fun _ -> test())
            )
            Game_lib_test.board_suite
         );
         "challenge critical section suite",
         (List.map
            (
              fun (name, speed, test) ->
              Alcotest_lwt.test_case
                name
                speed
                (fun _ _ -> test())
            )
            Challenge_test.challenge_suite
         );
       ]
