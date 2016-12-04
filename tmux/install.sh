#!/usr/bin/env bash

source ${ROOT=$(dirname $0)/..}/functions

declare -i failed

echo 'Installing tmux...'

[[ ! -d "$HOME/.tmux" ]] && mkdir -p "$HOME/.tmux"

# Tmux Plugin Manager
git_clone_or_update https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm || ((failed++))

exit $failed
