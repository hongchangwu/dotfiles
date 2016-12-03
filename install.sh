#!/usr/bin/env bash

ROOT=$(dirname $0)

DOT_FILES=(
  .aliases
  .bash_profile
  .emacs
  .functions
  .gitconfig
  .tmux.conf
  .vimrc
  )

copy()
{
  local src=$1
  local dst=$2
  [[ -f "$2" ]] && mv "$2" "$2.bak"
  cp "$1" "$2"
}

source "$ROOT/.functions"

if ! exists git
then
  echo "Couldn't find git!" 1>&2
  exit 1
fi

git clean -f

declare -i failed

echo 'Installing dotfiles...'
echo '(Your old files will be backed up with the suffix .bak)'
for f in "${DOT_FILES[@]}"
do
  copy "$ROOT/$f" "$HOME/$f" || ((failed++))
done

[[ ! -d "$HOME/bin" ]] && mkdir "$HOME/bin"
for f in $(find $ROOT/bin -type f -perm +111 -maxdepth 1)
do
  copy "$f" "$HOME/$f" || ((failed++))
done

if [[ -z $failed ]]
then
  echo "All done!"
else
  echo "Some commands have failed!"
  exit 1
fi
