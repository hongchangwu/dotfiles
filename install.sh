#!/usr/bin/env bash

source "./functions"

usage()
{
  echo "Usage: $0 [-n] [-h]"
}

while getopts "nh" arg
do
  case $arg in
    n) export NO_BACKUP=true
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

export ARCH=$(uname -s)

export ROOT=$(dirname $0)

DOT_FILES=(
  .aliases
  .bash_profile
  .emacs
  .functions
  .gitconfig
  .tmux.conf
  .vimrc
  )

export BACKUP_SUFFIX=".bak.$(date -u +%s)"

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

# Download Git auto-completion
download https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash ~/.git-completion.bash

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
