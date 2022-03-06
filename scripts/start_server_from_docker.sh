#!/bin/bash

set -o errexit
set -o nounset
set -o pipefail

set -o xtrace

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

${SCRIPT_DIR}/full_build_from_clean_state.sh

cd /root/gobblet
cd src/server

(
    cd config
    ln --symbolic local config
)

eval `opam env`

make test.byte
