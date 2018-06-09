FROM ubuntu:16.04

RUN apt-get update
RUN apt-get install sudo -y
RUN apt-get install git -y
RUN git clone https://github.com/bruce-ricard/gobblet.git ~/gobblet

ADD ./configure.sh /root
RUN /root/configure.sh

RUN chmod -R 777 /usr/local/bin
RUN apt-get install emacs -y
RUN cd ~/gobblet/src && eval `opam config env` && ./configure --enable-tests

ADD ttt-pg_hba.conf /etc/postgresql/9.5/main/pg_hba.conf

CMD cd ~/gobblet && ./run_tests.sh
