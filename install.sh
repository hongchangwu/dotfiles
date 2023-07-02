#!/usr/bin/env bash

set -euo pipefail

# Install Homebrew
if [[ ! -f /opt/homebrew/bin/brew ]]; then
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi
/opt/homebrew/bin/brew update && /opt/homebrew/bin/brew install coreutils reattach-to-user-namespace
export PATH="/opt/homebrew/opt/coreutils/libexec/gnubin:$PATH"

# Install Nix
sh <(curl -L https://releases.nixos.org/nix/nix-2.12.0/install) --no-channel-add --darwin-use-unencrypted-nix-store-volume
nix-channel --add https://nixos.org/channels/nixpkgs-23.05-darwin nixpkgs

DIR=$(dirname "$(readlink -f "$0")")
mkdir -p "$HOME/.config/"
[[ ! -d "$HOME/.config/nixpkgs" ]] && ln -s "$DIR/nixpkgs/" "$HOME/.config/nixpkgs"

# Install home-manager
nix-channel --add https://github.com/nix-community/home-manager/archive/release-23.05.tar.gz home-manager
nix-channel --update
nix-shell '<home-manager>' -A install

# Add system shells
[[ ! $(grep "$HOME/.nix-profile/bin/bash" /etc/shells) ]] && sudo sh -c 'echo "$HOME/.nix-profile/bin/bash" >> /etc/shells'
[[ ! $(grep "$HOME/.nix-profile/bin/zsh" /etc/shells) ]] && sudo sh -c 'echo "$HOME/.nix-profile/bin/zsh" >> /etc/shells'

# Copy straight.el lockfiles
mkdir -p "$HOME/.emacs.d/straight/"
[[ ! -d "$HOME/.emacs.d/straight/versions" ]] && cp -r "$DIR/straight/versions/" "$HOME/.emacs.d/straight"
