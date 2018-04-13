open OUnit2
open Lwt
open Ttt_common_lib_types

module CCS =
  Ttt_server_lib_challengeCriticalSection.Make(List_challenge_store)

let challenge =
  Ttt_server_lib_challenge.create "bob" (new id 42)

let create_challenge _ =
  let cs = CCS.create () in

  assert_equal [] (CCS.public_challenges_for_user cs "baz");
  ignore (
      CCS.add_challenge cs challenge >|=
        (fun () ->
          assert_equal 1 (List.length (CCS.public_challenges_for_user cs "baz"))
        )
    );
  ()

let suite =
  "challenges suite" >:::
    [
      "create challenge" >:: create_challenge
    ]

let () =
  run_test_tt_main suite
