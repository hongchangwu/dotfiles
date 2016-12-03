#!/usr/bin/env bash
# Toggle hidden files in Finder

ON=$(defaults read com.apple.finder AppleShowAllFiles)
[[ $ON -ne 0 ]] && defaults write com.apple.finder AppleShowAllFiles -bool false \
  || defaults write com.apple.finder AppleShowAllFiles -bool true
killall Finder
