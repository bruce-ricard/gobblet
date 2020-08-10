#!/bin/bash

set -ex

if [ "${INSERTED_GIT_REPO}" = 'true' ]; then
    echo 'Using inserted git repo'
else
    echo 'Pulling latest master...'
    cd ~/gobblet && git pull
fi

HASH=$(cd ~/gobblet && git show | head -1)
echo "Running tests on $HASH"

cd ~/gobblet/src
eval `opam config env`
rm -f setup.data
./configure --enable-tests

service postgresql start

cd ~/gobblet/src
psql -U postgres < dao/init_db.sql
psql gobblet -U postgres < dao/init_gobblet_db.sql
psql gobblet -U postgres < dao/set_users_permissions.sql

eval `opam config env`
. ~/gobblet/src/dao/before_build.sh

make
make install
(
    cd server
    make all
    make distclean
)
make test
