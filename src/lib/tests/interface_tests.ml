let sync_suite =
  List.map
    (fun (name, speed, test) ->
      Alcotest_lwt.test_case_sync
        name
        speed
        (fun _ -> test())
    )

(* let lwt_suite =
 *   List.map
 *      (
 *        fun (name, speed, test) ->
 *        Alcotest_lwt.test_case
 *          name
 *          speed
 *          (fun _ _ -> test())
 *      ) *)

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run
       "Interface tests"
       [
         "tic-tac-toe classical", (
           sync_suite Tic_tac_toe_classical_itest.suite
         );
       ]
