#!/bin/sh

apt-get update
apt-get install sudo
apt-get install git -y

cd
git clone https://github.com/bruce-ricard/gobblet.git
cd gobblet
./configure
