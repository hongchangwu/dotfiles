{ pkgs, ... }:

let
  pythonPackages = packages: with packages; [
    black
    pandas
    pip
    pylint
    pytest
    python-language-server
    rope
    setuptools
    yapf
  ] ++ (if pkgs.stdenv.isDarwin then [] else [powerline]);
  python = pkgs.python37.withPackages pythonPackages;
in
{
  fonts.fontconfig.enable = true;

  home = {
    file = {
      ".aliases".source = ./bash/aliases;
      ".emacs.d" = {
        source = ./emacs;
        recursive = true;
      };
      ".functions".source = ./bash/functions;
      ".local/bin" = {
        source = ./bin;
        recursive = true;
      };
      ".tmux.conf.osx".source = ./tmux/tmux.conf.osx;
      ".vim/colors/jellybeans.vim".source = builtins.fetchurl {
        url = "https://raw.githubusercontent.com/nanotech/jellybeans.vim/ef83bf4dc8b3eacffc97bf5c96ab2581b415c9fa/colors/jellybeans.vim";
        sha256 = "791e387b998fecb14169601ee4e925b8d0991da6a235d2a4ef4c2b3225c58f8c";
      };
    };

    packages = (with pkgs; [
      autoconf
      bzip2
      coreutils
      curl
      fontconfig
      gnum4
      htop
      nix-prefetch-scripts
      nixfmt
      nodejs
      ocaml
      ocamlformat
      openssl
      powerline-fonts
      powerline-go
      ripgrep
      rlwrap
      rnix-lsp
      silver-searcher
      tree
      wget
    ]) ++ (with pkgs.nodePackages; [
      bash-language-server
    ]) ++ (with pkgs.ocamlPackages; [
      dune_2
      findlib
      merlin
      ocp-indent
      utop
    ]) ++ [
      python
    ];

    sessionVariables = {
      EDITOR = "vim";
      LANG = "en_US.UTF-8";
      LANGUAGE = "en_US";
      PAGER = "less";
      TERM = "xterm-256color";
    };

    stateVersion = "20.03";
  };

  programs = {
    bash = {
      enable = true;
      profileExtra = builtins.readFile ./bash/profile;
    };

    emacs = {
      enable = true;
      extraPackages = epkgs: [ epkgs.all-the-icons ];
    };

    git = {
      enable = true;
      userName  = "Hongchang Wu";
      userEmail = "wuhc85@gmail.com";
      extraConfig = builtins.readFile ./git/config;
      ignores = [ "*~" "*.swp" "\\#*\\#" ".\\#*" ];
    };

    home-manager = {
      enable = true;
      path = "~/.nixpkgs";
    };

    opam.enable = true;

    tmux = {
      enable = true;
      extraConfig = builtins.readFile ./tmux/tmux.conf;
      tmuxinator.enable = true;
      plugins = with pkgs.tmuxPlugins; [
        resurrect
      ];
      secureSocket = false;
    };

    vim = {
      enable = true;
      extraConfig = builtins.readFile vim/vimrc;
      plugins = with pkgs.vimPlugins; [
        nerdtree
        fugitive
        syntastic
        taglist-vim
        vim-airline
        vim-airline-themes
      ];
    };

    zsh = {
      enable = true;
      enableCompletion = true;
      envExtra = builtins.readFile ./zsh/zshenv;
      profileExtra = builtins.readFile ./zsh/zprofile;
      initExtra = builtins.readFile ./zsh/zshrc;
      oh-my-zsh = {
        enable = true;
        plugins = [
          "cabal"
          "cargo"
          "docker"
          "docker-compose"
          "emacs"
          "git"
          "mix"
          "npm"
          "osx"
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
            rev = "0.4.4";
            sha256 = "1n9whlys95k4wc57cnz3n07p7zpkv796qkmn68a50ygkx6h3afqf";
          };
        }
        {
          name = "zsh-nix-shell";
          src = pkgs.fetchFromGitHub {
            owner = "chisui";
            repo = "zsh-nix-shell";
            rev = "v0.1.0";
            sha256 = "0snhch9hfy83d4amkyxx33izvkhbwmindy0zjjk28hih1a9l2jmx";
          };
        }
      ];
    };
  };

}
