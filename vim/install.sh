#!/usr/bin/env bash

declare -i failed

echo 'Installing vim...'

[[ ! -d "$HOME/.vim" ]] && mkdir -p "$HOME/.vim"

# Pathogen
mkdir -p ~/.vim/autoload ~/.vim/bundle && curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim || ((failed++))

# grb256
mkdir -p ~/.vim/colors && curl -LSso ~/.vim/colors/grb256.vim https://raw.githubusercontent.com/garybernhardt/dotfiles/master/.vim/colors/grb256.vim || ((failed++))

exit $failed
