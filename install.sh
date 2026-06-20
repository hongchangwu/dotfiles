#!/usr/bin/env bash

set -euo pipefail

NIX_VERSION="2.22.0"
NIXOS_VERSION="26.05"

# Install Homebrew
if [[ ! -f /opt/homebrew/bin/brew ]]; then
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi
/opt/homebrew/bin/brew update && /opt/homebrew/bin/brew install coreutils reattach-to-user-namespace
export PATH="/opt/homebrew/opt/coreutils/libexec/gnubin:$PATH"

# Install Nix
NIX_FIRST_BUILD_UID="351" sh <(curl -L "https://releases.nixos.org/nix/nix-${NIX_VERSION}/install") --daemon --no-channel-add
nix-channel --add "https://nixos.org/channels/nixpkgs-${NIXOS_VERSION}-darwin" nixpkgs

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
