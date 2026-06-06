# Repository Guide

This repository manages cross-platform dotfiles and developer tools with
Home Manager.

## Branches

- `main` is the Linux configuration branch.
- `mac` is the macOS configuration branch.
- Do not treat a `main` Home Manager build failure on macOS as a regression.
- Apply shared changes to both branches, normally by committing on the current
  branch and cherry-picking the commit onto the other branch.

## Layout

- `home-manager/home.nix`: packages, Home Manager programs, and dotfile links.
- `home-manager/emacs/`: Emacs configuration.
- `straight/versions/default.el`: pinned Emacs straight.el package versions.
- `home-manager/{bash,zsh,vim,neovim,tmux,...}`: tool-specific configuration.
- `install.sh`: macOS-oriented bootstrap using Nix channels and Homebrew.

This is a channel-based Home Manager setup, not a Nix flake. Package versions
come from the user's configured nixpkgs/Home Manager channels unless explicitly
pinned in the repository.

## Editing

- Keep platform-specific behavior on the appropriate branch.
- Prefer Home Manager/Nix for system tools; do not add Emacs-side package
  installers or other startup-time installation side effects.
- Prefer Emacs 30 built-ins over obsolete external packages where practical.
- When changing straight packages, update
  `straight/versions/default.el` and keep the live
  `~/.emacs.d/straight/versions/default.el` synchronized for testing.
- Do not commit generated Home Manager `result` links or straight build output.

## Verification

On the branch matching the current platform, validate Home Manager changes
with:

```sh
home-manager build -f home-manager/home.nix
```

Use `mac` on macOS and `main` on Linux. Do not use a build on the wrong
platform to assess whether the configuration is valid.

Use `home-manager switch -f home-manager/home.nix` only when live activation is
needed for testing.

For Emacs changes, run:

```sh
emacs --batch --debug-init
```

Also test a temporary daemon when changing package bootstrap or initialization,
because some failures only occur in daemon mode.

For shell changes, run:

```sh
shellcheck install.sh
```

Before committing, run `git diff --check` and confirm only intended files are
modified.
