#!/usr/bin/env bash

set -euo pipefail

# Install Nix
sh <(curl -L https://releases.nixos.org/nix/nix-2.12.0/install) --no-channel-add
nix-channel --add https://nixos.org/channels/nixos-23.05 nixpkgs

DIR=$(dirname "$(readlink -f "$0")")
mkdir -p "$HOME/.config/"
[[ ! -d "$HOME/.config/home-manager" ]] && ln -s "$DIR/home-manager/" "$HOME/.config/home-manager"

# Install home-manager
nix-channel --add https://github.com/nix-community/home-manager/archive/release-23.05.tar.gz home-manager
nix-channel --update
nix-shell '<home-manager>' -A install

# Copy straight.el lockfiles
mkdir -p "$HOME/.emacs.d/straight/"
[[ ! -d "$HOME/.emacs.d/straight/versions" ]] && cp -r "$DIR/straight/versions/" "$HOME/.emacs.d/straight"
