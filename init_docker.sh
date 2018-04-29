#!/bin/sh

sudo apt-get update
sudo apt-get install git -y
sudo apt-get install sudo -y

cd
git clone https://github.com/bruce-ricard/gobblet.git
cd gobblet
./configure
