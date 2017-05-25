#!/usr/bin/env bash

source ${ROOT=$(dirname $0)/..}/functions

declare -i failed

echo 'Installing bash-completion...'

[[ ! -d "$HOME/.local" ]] && mkdir -p "$HOME/.local"

git_clone_or_update https://github.com/scop/bash-completion.git "${TMPDIR=/tmp}/bash-completion" || ((failed++))
pushd $TMPDIR/bash-completion >/dev/null || ((failed++))
autoreconf -i >/dev/null || ((failed++))
./configure --prefix "$HOME/.local" >/dev/null || ((failed++))
make >/dev/null || ((failed++))
make install >/dev/null || ((failed++))
popd >/dev/null
rm -rf $TMPDIR/bash-completion &>/dev/null

exit $failed
