let config_file_name = "config/config"

let () =
  if Sys.file_exists config_file_name then
    Config_reader.group#read config_file_name
  else
    begin
      print_endline
        "[CRITICAL] Config file \"config/config\" couldn't be found. Aborting";
      exit 1
    end

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
