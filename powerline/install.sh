#!/usr/bin/env bash

source ${ROOT=$(dirname $0)/..}/functions

declare -i failed

echo 'Installing powerline...'

[[ ! -d "$HOME/.local/bin" ]] && mkdir -p "$HOME/.local/bin"
[[ ! -d "$HOME/.local/etc" ]] && mkdir -p "$HOME/.local/etc"

pip install --user powerline-status >/dev/null || ((failed++))

git_clone_or_update https://github.com/banga/powerline-shell.git ~/.local/etc/powerline-shell || ((failed++))
pushd ~/.local/etc/powerline-shell >/dev/null
perl -ne 'print unless m/username|hostname/' config.py.dist > config.py 2>/dev/null
./install.py >/dev/null || ((failed++))
ln -s ~/.local/etc/powerline-shell/powerline-shell.py ~/.local/bin/powerline-shell.py &>/dev/null
popd >/dev/null

exit $failed
