open Lwt
open Ttt_common_lib_types

module C = Ttt_server_lib_challenge

module CCS =
  Ttt_server_lib_challengeCriticalSection.Make(List_challenge_store)

let user1 = "bob"
let user2 = "bruce"
let user3 = "john"

let id1 = (new id 1)
let id2 = (new id 2)

let public_challenge1 =
  Ttt_server_lib_challenge.create user1 id1

let private_challenge1 =
  Ttt_server_lib_challenge.create user1 ~opponent:user2 id2

let other_players_can_see_public_challenge () =
  let challenges = CCS.create () in
  CCS.add_challenge challenges public_challenge1 >|=
    (fun () -> CCS.public_challenges_for_user challenges user2) >|=
    (function
     | [] -> failwith "We should have a challenge"
     | [c] -> Alcotest.(check int) "id is identical" id1#get_id (C.id c)#get_id
     | l -> failwith "There should be exactly one challenge"
    )

let challenged_players_can_see_private_challenge () =
  let challenges = CCS.create () in
  CCS.add_challenge challenges private_challenge1 >|=
    (fun () -> CCS.private_challenges_for_user challenges user2) >|=
    (function
     | [] -> failwith "We should have a challenge"
     | [c] -> Alcotest.(check int) "id is identical" id2#get_id (C.id c)#get_id
     | l -> failwith "There should be exactly one challenge"
    )

let you_should_not_see_your_own_challenge () =
  let challenges = CCS.create () in
  let add_pub = CCS.add_challenge challenges public_challenge1 in
  let add_priv = CCS.add_challenge challenges public_challenge1 in
  add_pub <&> add_priv >|=
    (fun () ->
      let publics = CCS.public_challenges_for_user challenges user1
      and privates = CCS.public_challenges_for_user challenges user1 in
      match publics, privates with
      | [], [] -> ()
      | [], _ -> failwith "you shouldn't see your own challenge as private"
      | _, [] -> failwith "you shouldn't see your own challenge as public"
      | _ ->  failwith "you shouldn't see your own challenge"
    )

let remove_challenge () =
  let challenges = CCS.create () in
  let add_pub = CCS.add_challenge challenges public_challenge1 in
  add_pub >>= (fun () -> CCS.remove_by_id challenges id1) >|=
    (let open Internal_types in
     function
     | Deleted(c) ->
        Alcotest.(check int) "id is identical" id1#get_id (C.id c)#get_id
     | Id_not_present -> failwith "game should be deleted"
    )

let not_remove_challenge () =
  let challenges = CCS.create () in
  let add_pub = CCS.add_challenge challenges public_challenge1 in
  add_pub >>= (fun () -> CCS.remove_by_id challenges id2) >|=
    (let open Internal_types in
     function
     | Deleted(c) ->
        failwith "This challenge shouldn't be deleted"
     | Id_not_present -> ()
    )

let challenge_suite = [
    "add public challenge", `Quick, other_players_can_see_public_challenge;
    "add private challenge", `Quick, challenged_players_can_see_private_challenge;
    "you can't see your own challenges", `Quick, you_should_not_see_your_own_challenge;
    "remove challenge by ID", `Quick, remove_challenge;
    "not remove other challenge", `Quick, not_remove_challenge;
  ]

let () =
  Alcotest.run
    "server lib"
    [
      "challenge critical section suite",
      List.map
        (
          fun (name, speed, test) ->
          Alcotest_lwt.test_case
            name
            speed
            (fun _ _ -> test())
        )
        challenge_suite;
    ]
