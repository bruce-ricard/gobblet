open Config_file

let group = new group

let string_option_cp = new option_cp string_wrappers
let int_option_cp = new option_cp int_wrappers

let socp p = string_option_cp ~group (["postgres"] @ [p]) None
let iocp p = int_option_cp ~group (["postgres"] @ [p]) None

let host = socp "host" "Postgres host"
let port = iocp "port" "Postgres port"
let database = socp "database" "Postgres database"
let user = socp "user" "Postgres user"
let password = socp "password" "Postgres password"
