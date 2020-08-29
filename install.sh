#!/usr/bin/env bash

set -euo pipefail

# Install Nix
curl -L https://nixos.org/nix/install | sh
. "$HOME/.nix-profile/etc/profile.d/nix.sh"

mkdir -p ~/.config/
cp -r nixpkgs/ ~/.config/

nix-shell ./default.nix -A install

sudo sh -c 'echo "$HOME/.nix-profile/bin/bash" >> /etc/shells'
sudo sh -c 'echo "$HOME/.nix-profile/bin/zsh" >> /etc/shells'
