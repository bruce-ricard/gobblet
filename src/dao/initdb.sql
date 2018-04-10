create table users
(
id varchar(30) not null primary key,
password_hash text not null,
email text
);

create table users_stats
(
id varchar(30) not null primary key references users,
created_at date not null,
last_connected_at date not null
);

create table tic_tac_toe_games
(
id serial not null primary key,
game text not null
);

create schema ratings;

create table ratings.tictactoeclassical
(
username varchar(30) not null primary key REFERENCES users (id),
rating numeric(6,2) not null,
rd numeric(5,2) not null,
sigma numeric(10,10) not null
);

create table ratings.tictactoexonly
(
username varchar(30) not null primary key REFERENCES users (id),
rating numeric(6,2) not null,
rd numeric(5,2) not null,
sigma numeric(10,10) not null,
);

create table ratings.three_men_morris
(
username varchar(30) not null primary key REFERENCES users (id),
rating numeric(6,2) not null,
rd numeric(5,2) not null,
sigma numeric(10,10) not null
)
