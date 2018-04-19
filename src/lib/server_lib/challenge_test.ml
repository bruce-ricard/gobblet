open Lwt
open Ttt_common_lib_types
open Test

module CCS =
  Ttt_server_lib_challengeCriticalSection.Make(List_challenge_store)

let challenge =
  Ttt_server_lib_challenge.create "bob" (new id 42)
