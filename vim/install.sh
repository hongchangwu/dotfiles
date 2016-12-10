#!/usr/bin/env bash

source ${ROOT=$(dirname $0)/..}/functions

declare -i failed

echo 'Installing vim...'

[[ ! -d "$HOME/.vim" ]] && mkdir -p "$HOME/.vim"

# Pathogen
mkdir -p ~/.vim/autoload ~/.vim/bundle
download https://raw.githubusercontent.com/tpope/vim-pathogen/master/autoload/pathogen.vim ~/.vim/autoload/pathogen.vim || ((failed++))

# grb256 color theme
mkdir -p ~/.vim/colors
download https://raw.githubusercontent.com/garybernhardt/dotfiles/master/.vim/colors/grb256.vim ~/.vim/colors/grb256.vim || ((failed++))

# Molokai color theme
download https://raw.githubusercontent.com/tomasr/molokai/master/colors/molokai.vim ~/.vim/colors/molokai.vim || ((failed++))

# Jellybeans color theme
download https://raw.githubusercontent.com/nanotech/jellybeans.vim/master/colors/jellybeans.vim ~/.vim/colors/jellybeans.vim || ((failed++))

# Tag List
git_clone_or_update https://github.com/vim-scripts/taglist.vim.git ~/.vim/bundle/taglist || ((failed++))

# NERD tree
git_clone_or_update https://github.com/scrooloose/nerdtree.git ~/.vim/bundle/nerdtree || ((failed++))

# Syntastic
git_clone_or_update https://github.com/vim-syntastic/syntastic.git ~/.vim/bundle/syntastic || ((failed++))

exit $failed
