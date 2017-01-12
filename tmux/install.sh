#!/usr/bin/env bash

source ${ROOT=$(dirname $0)/..}/functions

declare -i failed

echo 'Installing tmux...'

[[ ! -d "$HOME/.tmux" ]] && mkdir -p "$HOME/.tmux"

# Tmux Plugin Manager
git_clone_or_update https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm || ((failed++))

# Install plugins
~/.tmux/plugins/tpm/bin/install_plugins >/dev/null || ((failed++))

# Tmux auto-completion
download https://raw.githubusercontent.com/Bash-it/bash-it/master/completion/available/tmux.completion.bash ~/.tmux.completion.bash

# Tmuxinator auto-completion
download https://raw.githubusercontent.com/tmuxinator/tmuxinator/master/completion/tmuxinator.bash ~/.tmuxinator.bash

exit $failed
