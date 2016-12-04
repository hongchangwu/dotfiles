#!/usr/bin/env bash

copy()
{
  local src=$1
  local dst=$2
  [[ $NO_BACKUP != true ]] && [[ -f "$2" ]] && mv "$2" "$2$BACKUP_SUFFIX"
  cp "$1" "$2"
}

usage()
{
  echo "Usage: $0 [-n] [-h]"
}

while getopts "nh" arg
do
  case $arg in
    n) NO_BACKUP=true
       ;;
    h) usage
       exit 0
       ;;
    *) usage >&2
       exit 1
       ;;
  esac
done
shift $((OPTIND-1))

ARCH=$(uname -s)

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

BACKUP_SUFFIX=".bak.$(date -u '+%Y-%m-%dT%H:%M:%SZ')"

source "$ROOT/.functions"

if ! exists git
then
  echo "Couldn't find git!" >&2
  exit 1
fi

git clean -fdx &>/dev/null

declare -i failed

echo 'Installing dotfiles...'
[[ $NO_BACKUP != true ]] && echo "(Your old files will be backed up with the suffix $BACKUP_SUFFIX)"
for f in "${DOT_FILES[@]}"
do
  copy "$ROOT/$f" "$HOME/$f" || ((failed++))
done

[[ ! -d "$HOME/bin" ]] && mkdir "$HOME/bin"

if [[ $ARCH = Darwin ]]
then
  find_command="find $ROOT/bin -maxdepth 1 -type f -perm +111 -print"
elif [[ $ARCH = Linux ]]
then
  find_command="find $ROOT/bin -maxdepth 1 -type f -executable -print"
else
  find_command="find $ROOT/bin -type f -exec test -x {} \; -print"
fi

for f in $($find_command)
do
  copy "$f" "$HOME/$f" || ((failed++))
done

# Recursively install all components
for f in $(find "$ROOT" -mindepth 2 -maxdepth 2 -name install.sh -print)
do
  [[ -x "$f" ]] && "$f"
  [[ $? -ne 0 ]] && ((failed++))
done

if [[ -z $failed ]]
then
  echo "All done!"
else
  echo "Some commands have failed!" >&2
  exit 1
fi
