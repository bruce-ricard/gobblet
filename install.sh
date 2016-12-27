sudo apt-get update
yes | sudo apt-get install opam
opam init -y
eval `opam config env`
eval `opam config env`

opam switch 4.02.3
eval `opam config env`
eval `opam config env`
opam install depext -y
opam depext conf-gmp.1 -y && opam depext conf-m4.1 -y && opam depext conf-pkg-config.1.0 -y
opam depext conf-libpcre.1 -y && opam depext conf-openssl.1 -y && opam depext conf-zlib.1 -y && opam depext dbm.1.0 -y
opam install eliom.5.0.0 -y
opam install extlib -y

cd lib
oasis setup
make
sudo make install
cd ../server
make byte
sudo make install.byte
