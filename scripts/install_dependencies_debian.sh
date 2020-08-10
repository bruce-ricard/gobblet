#!/bin/bash

set -euxo pipefail

sudo apt-get update --yes
sudo apt-get install opam --yes
sudo apt-get install postgresql --yes
