#!/usr/bin/env bash

source ${ROOT=$(dirname $0)/..}/functions

declare -i failed

echo 'Installing fonts...'

git_clone_or_update https://github.com/powerline/fonts.git "${TMPDIR=/tmp}/fonts" || ((failed++))
$TMPDIR/fonts/install.sh >/dev/null || ((failed++))
rm -rf $TMPDIR/fonts &>/dev/null

exit $failed
