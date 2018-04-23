open Ttt_common_lib_types

module Make(PostgresConfig : PostgresConfiguration.CONFIG) =
  struct
    module Dao = UsersPostgresDao.Make(PostgresConfig)
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

    module XX(Archive : Internal_types.ARCHIVE) =
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

    module Challenges =
      Ttt_server_lib_challenge_api.Make
        (Ttt_server_lib_challengeCriticalSection.Make(List_challenge_store))
        (IdGenerator)

    module rec Games :
                 sig
                   include Ttt_server_lib_types.GAMES
                   include Internal_types.ARCHIVE
                 end =
      Ttt_server_lib_games.Make
        (Challenges)
        (MockGameArchiveDB)
        (GamesByIdAndUser)
        (GameList.TicTacToeClassical)
        (GameList.TicTacToeXOnly)
        (GameList.ThreeMenMorris)
        (Users)
       and GameList : Ttt_server_lib_types.GAME_LIST =
         Ttt_server_lib_game_list.Make(Dao)(Games)
end
