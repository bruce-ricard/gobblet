#!/bin/bash

set -eux

if [[ -n "${1:+x}" ]]; then
    docker run -e INSERTED_GIT_REPO=true -v "$1":/root/gooblet \
           -it brucericard/ttt:0.7
else
    docker run -e INSERTED_GIT_REPO=false \
           -it brucericard/ttt:0.7
fi
