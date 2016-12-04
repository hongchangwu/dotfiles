#!/usr/bin/env bash

source ${ROOT=$(dirname $0)/..}/functions

declare -i failed

echo 'Installing vim...'

[[ ! -d "$HOME/.vim" ]] && mkdir -p "$HOME/.vim"

# Pathogen
mkdir -p ~/.vim/autoload ~/.vim/bundle
download https://raw.githubusercontent.com/tpope/vim-pathogen/master/autoload/pathogen.vim ~/.vim/autoload/pathogen.vim || ((failed++))

# grb256
mkdir -p ~/.vim/colors
download https://raw.githubusercontent.com/garybernhardt/dotfiles/master/.vim/colors/grb256.vim ~/.vim/colors/grb256.vim || ((failed++))

exit $failed
