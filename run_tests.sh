#//bin/sh

set -ex

service postgresql start
sudo -u postgres createuser 'gobblet_server_user'

cd ~/gobblet/src && psql gobblet -U gobblet_server_user < dao/initdb.sql

eval `opam config env`
make
make test
make install
cd server
make all
