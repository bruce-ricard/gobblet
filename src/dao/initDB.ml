let create_tables dbh =
PGSQL(dbh) "create table users
(
id varchar(30) not null primary key,
password_hash text not null,
email text
)";
PGSQL(dbh) "create table users_stats
(
id varchar(30) not null primary key references users,
created_at date not null,
last_connected_at date not null
)
            ";
PGSQL(dbh) "create table tic_tac_toe_games
(
id serial not null primary key,
game text not null
)
"

let dbh = PGOCaml.connect ()
let () = create_tables dbh
