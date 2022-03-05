#!/bin/bash

set -ex

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )


if [ "${INSERTED_GIT_REPO}" = 'true' ]; then
    echo 'Using inserted git repo'
else
    echo 'Pulling latest master...'
    cd ~/gobblet && git pull
fi

HASH=$(cd ~/gobblet && git show | head -1)
echo "Running tests on $HASH"

${SCRIPT_DIR}/full_build_from_clean_state.sh

eval `opam env`

(
    cd src/server
    make distclean
)
(
    cd src
    make test
)
