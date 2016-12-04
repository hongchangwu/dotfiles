#!/usr/bin/env bash

declare -i failed

echo 'Installing tmux...'

[[ ! -d "$HOME/.tmux" ]] && mkdir -p "$HOME/.tmux"

if [[ -d ~/.tmux/plugins/tpm ]]
then
  pushd ~/.tmux/plugins/tpm >/dev/null
  git pull --quiet || ((failed++))
  popd >/dev/null
else
  git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm &>/dev/null || ((failed++))
fi

exit $failed
