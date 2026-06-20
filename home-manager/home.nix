{ config, pkgs, ... }:

let
  pythonPackages = packages: with packages; [
    black
    pandas
    pip
    polars
    pylint
    pytest
    rope
    setuptools
    yapf
  ];
  python = pkgs.python312.withPackages pythonPackages;
  vimPlugins = with pkgs.vimPlugins; [
    nerdtree
    nord-vim
    vim-fugitive
    vim-airline
    vim-commentary
    vim-easymotion
  ];
  neovimPlugins = with pkgs.vimPlugins; [
    nvim-lspconfig
    nvim-treesitter
  ];
in
{
  fonts.fontconfig.enable = true;

  home = {
    username = builtins.getEnv "USER";

    homeDirectory = builtins.getEnv "HOME";

    file = {
      ".aliases".source = ./bash/aliases;
      ".aspell.conf".text = ''data-dir ${config.home.homeDirectory}/.nix-profile/lib/aspell
master en_US
extra-dicts en-computers.rws
'';
      ".aws/config".text = ''[default]
region=us-east-1
output=json
'';
      ".dir_colors".source = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/arcticicestudio/nord-dircolors/v0.2.0/src/dir_colors";
        sha256 = "0a6i9pvl4lj2k1snmc5ckip86akl6c0svzmc5x0vnpl4id0f3raw";
      };
      ".emacs.d" = {
        source = ./emacs;
        recursive = true;
      };
      ".functions".source = ./bash/functions;
      ".local/bin" = {
        source = ./bin;
        recursive = true;
      };
      ".config/tmux/tmux.conf.osx".source = ./tmux/tmux.conf.osx;
    };

    packages = (with pkgs; [
      aspell
      aspellDicts.en
      aspellDicts.en-computers
      autoconf
      automake
      bash-language-server
      bat
      bzip2
      cachix
      coreutils
      curl
      dockerfile-language-server
      entr
      fd
      fontconfig
      gh
      git-lfs
      glibcLocales
      gnum4
      go
      hanazono
      htop
      jq
      locale
      niv
      nix-prefetch-scripts
      nixfmt
      nodejs
      ocaml
      ocamlformat
      openssl
      pkg-config
      poetry
      powerline-fonts
      powerline-go
      pyright
      python
      ripgrep
      rlwrap
      rustup
      shellcheck
      silver-searcher
      tree
      tree-sitter
      wget
    ]) ++ (with pkgs.haskellPackages; [
      hlint
      ormolu
    ]) ++ (with pkgs.ocamlPackages; [
      dune_3
      findlib
      merlin
      ocp-indent
      utop
    ]);

    sessionVariables = {
      EDITOR = "vim";
      LANG = "en_US.UTF-8";
      LANGUAGE = "en_US";
      LD_LIBRARY_PATH = "${pkgs.stdenv.cc.cc.lib}/lib";
      LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
      PAGER = "less";
      TERM = "xterm-256color";
    };

    stateVersion = "26.05";
  };

  nixpkgs.overlays = [
    (final: prev: {
      direnv = prev.direnv.overrideAttrs (_: {
        doCheck = false;
      });
    })
  ];

  xdg.configFile."nix/nix.conf".text = ''
    experimental-features = nix-command flakes
  '';

  programs = {
    bash = {
      enable = true;
      profileExtra = builtins.readFile ./bash/profile;
    };

    direnv = {
      enable = true;
    };


    emacs = {
      enable = true;
      extraPackages = epkgs: [ epkgs.all-the-icons ];
    };

    git = {
      enable = true;
      settings = import ./git/config.nix;
      ignores = [ "*~" "*.swp" "\\#*\\#" ".\\#*" "*.bak" "*.tmp" "nohup.out" ".vscode/" ];
    };

    home-manager = {
      enable = true;
    };

    neovim = {
      enable = true;
      extraConfig = builtins.readFile vim/vimrc;
      initLua = builtins.readFile neovim/init.lua;
      plugins = builtins.filter (pkg: pkg.pname != "vim-fugitive") vimPlugins ++ neovimPlugins;
    };

    opam.enable = true;

    tmux = {
      enable = true;
      extraConfig = builtins.readFile ./tmux/tmux.conf;
      tmuxinator.enable = true;
      plugins = with pkgs.tmuxPlugins; [
        nord
        prefix-highlight
        resurrect
        {
          plugin = continuum;
          extraConfig = "set -g @continuum-restore 'on'";
        }
      ];
      secureSocket = false;
    };

    vim = {
      enable = true;
      extraConfig = builtins.readFile vim/vimrc;
      plugins = vimPlugins;
    };

    zsh = {
      enable = true;
      enableCompletion = true;
      envExtra = builtins.readFile ./zsh/zshenv;
      profileExtra = builtins.readFile ./zsh/zprofile;
      initContent = builtins.readFile ./zsh/zshrc;
      oh-my-zsh = {
        enable = true;
        plugins = [
          "cabal"
          "docker"
          "docker-compose"
          "git"
          "mix"
          "npm"
          "rust"
          "sbt"
          "scala"
          "stack"
          "tmux"
          "tmuxinator"
          "vagrant"
        ];
      };
      plugins = [
        {
          name = "nix-zsh-completions";
          src = pkgs.fetchFromGitHub {
            owner = "spwhitt";
            repo = "nix-zsh-completions";
            rev = "0.5.1";
            sha256 = "0pzqng23z21h5w9b3mv53b1jaivdjvq7pm2bfsh0m2pah5rwq1kf";
          };
        }
        {
          name = "zsh-nix-shell";
          src = pkgs.fetchFromGitHub {
            owner = "chisui";
            repo = "zsh-nix-shell";
            rev = "v0.8.0";
            sha256 = "1lzrn0n4fxfcgg65v0qhnj7wnybybqzs4adz7xsrkgmcsr0ii8b7";
          };
        }
      ];
    };
  };
}
