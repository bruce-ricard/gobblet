module ServerLib = Ttt_server_lib.Make(Parsed_config.PostgresConfig)

module Games = ServerLib.Games
module Users = ServerLib.Users
module GameList = ServerLib.GameList
