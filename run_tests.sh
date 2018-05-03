#//bin/sh

set -ex

service postgresql start
sudo -u postgres createdb gobblet
sudo -u postgres createuser 'gobblet_server_user'

cd ~/gobblet/src && psql gobblet -U gobblet_server_user < dao/initdb.sql

eval `opam config env`
source dao/before_build.sh
make
make test
make install
cd server
make all
