#!/bin/bash

set -euxo pipefail

sudo apt-get update --yes
sudo apt-get install opam --yes

sudo apt-get install curl --yes
ln -snf "/usr/share/zoneinfo/$(curl https://ipapi.co/timezone)" /etc/localtime
sudo apt-get install postgresql --yes
