module ServerLib = Ttt_server_lib.Make(Config_parser.PostgresConfig)

module Games = ServerLib.Games
module Users = ServerLib.Users
module GameList = ServerLib.GameList
