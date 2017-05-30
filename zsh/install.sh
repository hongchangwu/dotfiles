#!/usr/bin/env bash

source ${ROOT=$(dirname $0)/..}/functions

declare -i failed

echo 'Installing zsh...'

git_clone_or_update https://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh || ((failed++))

exit $failed
