#!/bin/bash

set -ex

cd ~/gobblet/src
eval `opam env`
rm -f setup.data
./configure --enable-tests

service postgresql start

cd ~/gobblet/src
psql -U postgres < dao/init_db.sql
psql gobblet -U postgres < dao/init_gobblet_db.sql
psql gobblet -U postgres < dao/set_users_permissions.sql

eval `opam env`
. ~/gobblet/src/dao/before_build.sh

make
make install
(
    cd server
    make all
)
