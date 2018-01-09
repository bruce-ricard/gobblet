#/bin/sh

set -e

ocamlfind remove ttt-daos
ocamlfind remove ttt-server-lib
ocamlfind remove ttt-game-lib
ocamlfind remove ttt-user-lib
ocamlfind remove ttt-common-lib


cd lib/common
make distclean
make
make install
cd ../..

cd dao
make distclean
make
make install
cd ..

cd lib/game_lib
make distclean
make
make install
cd ../..

cd lib/user_lib
make distclean
make
make install
cd ../..

cd lib/server_lib
make distclean
make
make install
cd ../..

cd server
make distclean
make byte
