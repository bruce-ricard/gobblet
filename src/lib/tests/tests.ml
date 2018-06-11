let () =
  Alcotest.run
    "game lib"
    [
      "board suite", Game_lib_test.board_suite;
      "challenge critical section suite",
      List.map
        (
          fun (name, speed, test) ->
          Alcotest_lwt.test_case
            name
            speed
            (fun _ _ -> test())
        )
        Challenge_test.challenge_suite;

    ]
