let () = Config_reader.group#read "config/config"

module ServerConfig =
  struct
    open Config_reader.ServerConfig
    let log_level =
      match log_level#get with
      | "ERROR" -> Logs.Error
      | "WARNING" -> Logs.Warning
      | "INFO" -> Logs.Info
      | "DEBUG" -> Logs.Debug
      | s -> failwith (Printf.sprintf "Invalid log level '%s'" s)
  end

module PostgresConfig : PostgresConfiguration.CONFIG =
  struct
    open Config_reader.PostgresConfig
    let host = host#get
    let port = port#get
    let user = user#get
    let password = password#get
    let database = database#get
  end
