FROM ubuntu:16.04

RUN apt-get update
RUN apt-get install sudo -y
RUN apt-get install git -y
RUN git clone https://github.com/bruce-ricard/gobblet.git ~/gobblet
RUN cd ~/gobblet && ./configure.sh

RUN cd ~ && git clone https://github.com/bruce-ricard/o-glicko2.git
RUN chmod -R 777 /usr/local/bin
RUN cd ~/o-glicko2 && eval `opam config env` && make && make install

CMD ~/gobblet/run_tests.sh
