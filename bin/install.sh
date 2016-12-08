#!/usr/bin/env bash

source ${ROOT=$(dirname $0)/..}/functions

declare -i failed

echo 'Installing bin...'

[[ ! -d "$HOME/.local/bin" ]] && mkdir -p "$HOME/.local/bin"

if [[ $ARCH = Darwin ]]
then
  find_command="find $ROOT/bin -maxdepth 1 -type f -perm +111 -print"
elif [[ $ARCH = Linux ]]
then
  find_command="find $ROOT/bin -maxdepth 1 -type f -executable -print"
else
  find_command="find $ROOT/bin -type f -exec test -x {} ; -print"
fi

for f in $($find_command)
do
  [[ $(basename "$f") = install.sh ]] && continue
  copy "$f" "$HOME/.local/$f" || ((failed++))
done

exit $failed
