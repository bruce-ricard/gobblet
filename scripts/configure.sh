#!/bin/bash

set -euxo pipefail

opam init -y
opam update
eval `opam config env`
eval `opam config env`
opam switch 4.02.3
eval `opam config env`
eval `opam config env`
opam install depext -y
opam depext \
	 conf-gmp.1 \
	 conf-m4.1 \
	 conf-pkg-config.1.0 \
	 conf-libpcre.1 \
	 conf-openssl.1 \
	 conf-zlib.1 \
	 dbm.1.2 \
	 sqlite3.4.1.3 -y

opam install -y \
	 dbm \
	 eliom.5.0.0 \
	 extlib \
	 sha \
	 fmt \
	 logs \
	 core \
	 pgocaml \
	 config-file \
	 alcotest \
	 alcotest-lwt \
	 glicko2
