#!/bin/sh

set -ex

service postgresql start
sudo -u postgres createdb gobblet

cd ~/gobblet/src && psql gobblet -U postgres < dao/initdb.sql

eval `opam config env`
source ~/gobblet/src/dao/before_build.sh
export PGUSER='postgres'
make
make test
make install
cd server
make all
