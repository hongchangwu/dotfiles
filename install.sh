#!/usr/bin/env bash

set -euo pipefail

NIX_VERSION="2.22.0"
NIXOS_VERSION="25.11"

# Install Nix
sh <(curl -L "https://releases.nixos.org/nix/nix-${NIX_VERSION}/install") --daemon --no-channel-add
nix-channel --add "https://nixos.org/channels/nixos-${NIXOS_VERSION}" nixpkgs

DIR=$(dirname "$(readlink -f "$0")")
mkdir -p "$HOME/.config/"
[[ ! -d "$HOME/.config/home-manager" ]] && ln -s "$DIR/home-manager/" "$HOME/.config/home-manager"

# Install home-manager
nix-channel --add "https://github.com/nix-community/home-manager/archive/release-${NIXOS_VERSION}.tar.gz" home-manager
nix-channel --update
nix-shell '<home-manager>' -A install

# Copy straight.el lockfiles
mkdir -p "$HOME/.emacs.d/straight/"
rm -rf "$HOME/.emacs.d/straight/versions"
cp -r "$DIR/straight/versions/" "$HOME/.emacs.d/straight"
