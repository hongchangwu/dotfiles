#!/usr/bin/env bash

source ${ROOT=$(dirname $0)/..}/functions

declare -i failed

echo 'Installing emacs...'

[[ ! -d "$HOME/.emacs.d/" ]] && mkdir -p "$HOME/.emacs.d/"
cp "$(dirname $0)/init.el" "$HOME/.emacs.d/" || ((failed++))
cp -r "$(dirname $0)/site-lisp" "$HOME/.emacs.d/" || ((failed++))
[[ ! -d "$HOME/.emacs.d/straight/" ]] && mkdir -p "$HOME/.emacs.d/straight/"
cp -r "$(dirname $0)/straight/versions" "$HOME/.emacs.d/straight/" || ((failed++))

# HLint
[[ ! -d "$HOME/.emacs.d/site-lisp/hlint" ]] && mkdir -p "$HOME/.emacs.d/site-lisp/hlint"
download https://raw.githubusercontent.com/ndmitchell/hlint/master/data/hs-lint.el ~/.emacs.d/site-lisp/hlint/hs-lint.el || ((failed++))

exit $failed
