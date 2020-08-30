#!/usr/bin/env bash

set -euo pipefail

# Install Nix
curl -L https://nixos.org/nix/install | sh
. "$HOME/.nix-profile/etc/profile.d/nix.sh"

if [[ $(uname -s) = Darwin ]]; then
  READLINK=greadlink
else
  READLINK=readlink
fi
DIR=$(dirname "$($READLINK -f "$0")")
mkdir -p "$HOME/.config/"
ln -s "$DIR/nixpkgs/" "$HOME/.config/nixpkgs"

# Install home-manager
nix-shell ./default.nix -A install
ln -s $(nix eval --raw '(builtins.fetchTarball (import ./home-manager.nix))') ~/.config/nixpkgs/home-manager

# Add system shells
[[ ! $(grep "$HOME/.nix-profile/bin/bash" /etc/shells) ]] && sudo sh -c 'echo "$HOME/.nix-profile/bin/bash" >> /etc/shells'
[[ ! $(grep "$HOME/.nix-profile/bin/zsh" /etc/shells) ]] && sudo sh -c 'echo "$HOME/.nix-profile/bin/zsh" >> /etc/shells'
