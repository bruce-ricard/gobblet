sudo apt-get update
yes | sudo apt-get install opam
yes | opam init
eval `opam config env`
eval `opam config env`

opam switch 4.02.3
eval `opam config env`
eval `opam config env`
opam install depext
yes | opam depext conf-gmp.1 && yes | opam depext conf-m4.1 && yes | opam depext conf-pkg-config.1.0
yes | opam depext conf-libpcre.1 && yes | opam depext conf-openssl.1 && yes | opam depext conf-zlib.1 && yes | opam depext dbm.1.0
yes | opam install eliom.5.0.0
yes | opam install extlib

cd lib
oasis setup
make
sudo make install
cd ../server
make test.byte
