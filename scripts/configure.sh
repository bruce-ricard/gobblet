#!/bin/bash

set -euxo pipefail

opam init -y
eval `opam config env`
eval `opam config env`
opam switch 4.02.3
eval `opam config env`
eval `opam config env`
opam install depext -y
opam depext conf-gmp.1 -y && opam depext conf-m4.1 -y && opam depext conf-pkg-config.1.0 -y
opam depext conf-libpcre.1 -y && opam depext conf-openssl.1 -y && opam depext conf-zlib.1 -y && opam depext dbm.1.1 -y
opam depext sqlite3.4.1.3 -y
opam install dbm -y
opam install eliom.5.0.0 -y
opam install extlib -y
opam install sha -y
opam install fmt -y
opam install logs -y
opam install core -y
opam install pgocaml -y
opam install config-file -y
opam install alcotest -y
opam install alcotest-lwt -y

opam install glicko2 -y
