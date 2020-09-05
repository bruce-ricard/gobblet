let sync_suite =
  List.map
    (fun (name, speed, test) ->
      Alcotest_lwt.test_case_sync
        name
        speed
        (fun _ -> test())
    )

let lwt_suite =
  List.map
     (
       fun (name, speed, test) ->
       Alcotest_lwt.test_case
         name
         speed
         (fun _ _ -> test())
     )


let () =
  Lwt_main.run
  @@ Alcotest_lwt.run
       "game lib"
       [
         "board suite", (
           sync_suite Game_lib_test.board_suite
         );
         "challenge critical section suite", (
           lwt_suite Challenge_test.challenge_suite
         );
       ]
