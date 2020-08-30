FROM ubuntu:20.04

RUN apt-get update

RUN apt-get install curl --yes
RUN ln -snf "/usr/share/zoneinfo/$(curl https://ipapi.co/timezone)" /etc/localtime

RUN apt-get install sudo -y
RUN apt-get install git -y
RUN git clone https://github.com/bruce-ricard/gobblet.git ~/gobblet
RUN chmod -R 777 /usr/local/bin

RUN apt-get install emacs --yes
RUN apt-get install opam --yes
RUN apt-get install postgresql --yes


RUN opam init --disable-sandboxing --yes
RUN opam update

RUN eval `opam env` && opam switch create 4.11.0

RUN eval `opam env` && opam install depext --yes

ENV OPAMYES true

RUN apt-get install --yes libgdbm-dev
RUN apt-get install --yes pkg-config libgmp-dev libpcre3-dev libssl-dev zlib1g-dev

RUN eval `opam env` && opam install	 core
RUN eval `opam env` && opam install	 alcotest
RUN eval `opam env` && opam install	 alcotest-lwt
RUN eval `opam env` && opam install	 glicko2
RUN eval `opam env` && opam install	 fmt
RUN eval `opam env` && opam install	 logs
RUN eval `opam env` && opam install	 config-file
RUN eval `opam env` && opam install	 dbm
RUN eval `opam env` && opam install	 pgocaml
RUN eval `opam env` && opam install	 pgocaml_ppx
RUN eval `opam env` && opam install	 extlib
RUN eval `opam env` && opam install	 sha
RUN eval `opam env` && opam install	 eliom.6.12.1


RUN cd ~/gobblet/src && eval `opam env` && ./configure --enable-tests

ADD conf/ttt-pg_hba.conf /etc/postgresql/9.5/main/pg_hba.conf

CMD cd ~/gobblet && ./scripts/run_tests.sh
