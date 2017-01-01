#!/usr/bin/env bash

source ${ROOT=$(dirname $0)/..}/functions

declare -i failed

echo 'Installing emacs...'

[[ ! -d "$HOME/.emacs.d/site-lisp" ]] && mkdir -p "$HOME/.emacs.d/site-lisp"

# HLint
[[ ! -d "$HOME/.emacs.d/site-lisp/hlint" ]] && mkdir -p "$HOME/.emacs.d/site-lisp/hlint"
download https://raw.githubusercontent.com/ndmitchell/hlint/master/data/hs-lint.el ~/.emacs.d/site-lisp/hlint/hs-lint.el || ((failed++))

exit $failed
