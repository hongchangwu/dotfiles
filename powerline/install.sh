#!/usr/bin/env bash

source ${ROOT=$(dirname $0)/..}/functions

declare -i failed

echo 'Installing powerline...'

[[ ! -d "$HOME/.local/bin" ]] && mkdir -p "$HOME/.local/bin"
[[ ! -d "$HOME/.local/etc" ]] && mkdir -p "$HOME/.local/etc"

pip install --user powerline-status >/dev/null || ((failed++))

pip install --user powerline-shell >/dev/null || ((failed++))

exit $failed
