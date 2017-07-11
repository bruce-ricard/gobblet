ocamlfind remove ttt-game-lib
ocamlfind remove ttt-user-lib
ocamlfind remove ttt-server-lib


cd lib/game_lib
make distclean
make
make install

cd ../user_lib
make distclean
make
make install

cd ../server_lib
make distclean
make
make install

cd ../../server
make distclean
make
