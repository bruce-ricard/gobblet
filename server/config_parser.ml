let () = Config_reader.group#read "config/config"

module PostgresConfig : PostgresConfiguration.CONFIG =
  struct
    open Config_reader
    let host = host#get
    let port = port#get
    let user = user#get
    let password = password#get
    let database = database#get
  end
