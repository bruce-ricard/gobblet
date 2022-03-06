#!/bin/bash

set -o errexit
set -o nounset
set -o pipefail

set -o xtrace

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

docker run \
       -it \
       --publish 8080:8080 \
       ttt \
       /root/gobblet/scripts/start_server_from_docker.sh
