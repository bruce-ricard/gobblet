open Ttt_common_lib_types

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

module Dao = UsersPostgresDao.Make(Config_parser.PostgresConfig)

module Users = Ttt_user_lib_users.Make(Dao)

module MockGameArchiveDB =
  struct
    let put_game id game =
      Logs.err (fun m -> m "Mock storing game %d into archive DB" id#get_id);
      ()

    let get_game id =
      Logs.err (fun m -> m "Mock getting game %d from archive DB" id#get_id);
      None

    let get_games_for_user user =
      Logs.err (fun m -> m "Mock getting games for user %s" user);
      []
  end

module IdGenerator =
  struct
    let current = ref 0

    let next () =
      incr current;
      new id (!current)
  end

module XX(Archive : Ttt_server_lib_types.ARCHIVE) =
  struct
    module GameList = Ttt_server_lib_game_list.Make(Dao)(Archive)

    module TicTacToeClassical =
      GameList.TicTacToeClassical

    module TicTacToeXOnly =
      GameList.TicTacToeXOnly

    module ThreeMenMorris =
      GameList.ThreeMenMorris
  end

module GamesByIdAndUser =
  Ttt_server_lib_game_store.GamesByIdAndUser

module rec Games :
             sig
               include Ttt_server_lib_types.GAMES
               include Ttt_server_lib_types.ARCHIVE
             end =
  Ttt_server_lib_games.Make
    (Ttt_server_lib_challenge_store)
    (IdGenerator)
    (MockGameArchiveDB)
    (GamesByIdAndUser)
    (GameList.TicTacToeClassical)
    (GameList.TicTacToeXOnly)
    (GameList.ThreeMenMorris)
    (Users)
   and GameList : Ttt_server_lib_game_list.GAME_LIST =
     Ttt_server_lib_game_list.Make(Dao)(Games)

module TicTacToeClassical =
  GameList.TicTacToeClassical

module TicTacToeXOnly =
  GameList.TicTacToeXOnly

module ThreeMenMorris =
  GameList.ThreeMenMorris

let current_user =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    (None : (string * Ttt_user_lib_user.user) option)

let message_next_page =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    (None : string option)

let set_message_next_page m : unit =
  Lwt.async (fun () -> Eliom_reference.set message_next_page (Some m))

let instant_message_ref =
  Eliom_reference.eref
    ~scope:Eliom_common.default_process_scope
    (None :
       (string React.event * ((?step:React.step -> string -> unit))) option
    )
