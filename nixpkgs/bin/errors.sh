#!/usr/bin/env bash
# Find compiler errors in Jenkins log

if [ "$#" -eq "1" ]
then
  user=`whoami`
  url="$1"
  curl -s --anyauth -u $user $url | perl -ne 'if (m/^.*\.(scala|java):[0-9]+:[ ]error:/m) { $m = 1 } elsif ($m && m/^[^ ]/m) { $m = 0 }; print if $m'
else
  echo "Usage: `basename $0` URL"
fi
