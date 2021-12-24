#!/usr/bin/env bash

set -euo pipefail

# Install Nix
sh <(curl -L https://releases.nixos.org/nix/nix-2.3.16/install) --no-channel-add
. "$HOME/.nix-profile/etc/profile.d/nix.sh"
nix-channel --add https://nixos.org/channels/nixos-21.11 nixpkgs

DIR=$(dirname "$(readlink -f "$0")")
mkdir -p "$HOME/.config/"
[[ ! -d "$HOME/.config/nixpkgs" ]] && ln -s "$DIR/nixpkgs/" "$HOME/.config/nixpkgs"

# Install home-manager
nix-channel --add https://github.com/rycee/home-manager/archive/release-21.11.tar.gz home-manager
nix-channel --update
nix-shell '<home-manager>' -A install

# Add system shells
[[ ! $(grep "$HOME/.nix-profile/bin/bash" /etc/shells) ]] && sudo sh -c 'echo "$HOME/.nix-profile/bin/bash" >> /etc/shells'
[[ ! $(grep "$HOME/.nix-profile/bin/zsh" /etc/shells) ]] && sudo sh -c 'echo "$HOME/.nix-profile/bin/zsh" >> /etc/shells'

# Copy straight.el lockfiles
mkdir -p "$HOME/.emacs.d/straight/"
[[ ! -d "$HOME/.emacs.d/straight/versions" ]] && cp -r "$DIR/straight/versions/" "$HOME/.emacs.d/straight"
