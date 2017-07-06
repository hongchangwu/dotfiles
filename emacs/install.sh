#!/usr/bin/env bash

source ${ROOT=$(dirname $0)/..}/functions

declare -i failed

echo 'Installing emacs...'

[[ ! -d "$HOME/.emacs.d" ]] && mkdir -p "$HOME/.emacs.d"
cp "$(dirname $0)/init.el" "$(dirname $0)/Cask" "$HOME/.emacs.d" || ((failed++))
cp -r "$(dirname $0)/site-lisp" "$HOME/.emacs.d" || ((failed++))

if [[ ! -d "$HOME/.cask" ]]; then
  curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python &>/dev/null
fi

pushd ~/.emacs.d >/dev/null
~/.cask/bin/cask install >/dev/null || ((failed++))
popd >/dev/null

# HLint
[[ ! -d "$HOME/.emacs.d/site-lisp/hlint" ]] && mkdir -p "$HOME/.emacs.d/site-lisp/hlint"
download https://raw.githubusercontent.com/ndmitchell/hlint/master/data/hs-lint.el ~/.emacs.d/site-lisp/hlint/hs-lint.el || ((failed++))

exit $failed
