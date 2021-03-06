#!/bin/bash

set -euxo pipefail

opam init --disable-sandboxing --yes
opam update
eval `opam env`
opam switch create 4.11.0
eval `opam env`
opam install depext --yes
opam depext --yes \
	 conf-gmp.1 \
	 conf-m4.1 \
	 conf-pkg-config.1.0 \
	 conf-libpcre.1 \
	 conf-openssl.1 \
	 conf-zlib.1 \
	 dbm.1.2 \
	 sqlite3.4.1.3

opam install --yes \
	 dbm \
	 eliom.6.12.1 \
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
