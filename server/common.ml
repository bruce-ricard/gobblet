module RDB : Ttt_server_lib_types.REACT_DB = functor (T : sig type t end) ->
  struct
    let objects = ref []

    let put id t = objects := (id,t) :: !objects
    let delete id = objects := List.filter (fun (x,_) -> x <> id) !objects

    let get id =
      try
        Some (List.assoc id !objects)
      with
        Not_found -> None

    let get_channel id =
      Core.Std.Option.map (get id) ~f:fst

    let get_update_function id =
      match get id with
      | Some (_,f) -> f
      | None -> failwith "no such ID"
  end

module Users = Ttt_user_lib_users.Make(PostgresDao)

module Tic_tac_toe_classical =
  Ttt_server_lib_games.Make
    (Ttt_game_lib_games.TicTacToeClassical)
    (Ttt_game_lib_pieces.XOPiece)
    (RDB)

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let ts = Core.Time.(to_string @@ now ()) in
    msgf @@ fun ?header ?tags fmt ->
            Format.kfprintf k ppf ("[%s]%a @[" ^^ fmt ^^ "@]@.")
                            ts Logs.pp_header (level, header)
  in
  { Logs.report = report }

let init_logs () =
  Logs.set_reporter (reporter (Format.std_formatter));
  Logs.set_level (Some Logs.Debug)

let () = init_logs (); Logs.info (fun m -> m "logs initialized")

let current_user =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    (None : (string * Ttt_user_lib_user.user) option)

let message_next_page =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    (None : string option)

let set_message m : unit =
  Lwt.async (fun () -> Eliom_reference.set message_next_page (Some m))

            (* difference between Lwt.async and ignore?*)
