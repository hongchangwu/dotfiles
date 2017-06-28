#!/usr/bin/env bash

source ${ROOT=$(dirname $0)/..}/functions

declare -i failed

echo 'Installing emacs...'

[[ ! -d "$HOME/.emacs.d" ]] && mkdir -p "$HOME/.emacs.d"
[[ ! -d "$HOME/.emacs.d/site-lisp" ]] && mkdir -p "$HOME/.emacs.d/site-lisp"
cp $(dirname $0)/Cask "$HOME/.emacs.d" || ((failed++))
for file in $(dirname $0)/*.el; do
  if [[ "$file" =~ init.el$ ]]; then
    cp "$file" "$HOME/.emacs.d" || ((failed++))
  else
    cp "$file" "$HOME/.emacs.d/site-lisp" || ((failed++))
  fi
done

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
