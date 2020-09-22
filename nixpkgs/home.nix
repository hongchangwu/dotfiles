{ config, pkgs, ... }:

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
  ];
  python = pkgs.python37.withPackages pythonPackages;
  nord-tmux = pkgs.tmuxPlugins.mkDerivation rec {
    pluginName = "nord";
    version = "0.3.0";
    src = pkgs.fetchFromGitHub {
      owner = "arcticicestudio";
      repo = "nord-tmux";
      rev = "v${version}";
      sha256 = "14xhh49izvjw4ycwq5gx4if7a0bcnvgsf3irywc3qps6jjcf5ymk";
    };
  };
  vimPlugins = with pkgs.vimPlugins; [
    nerdtree
    nord-vim
    fugitive
    syntastic
    vim-airline
    vim-commentary
    vim-easymotion
  ];
in
{
  fonts.fontconfig.enable = true;

  home = {
    file = {
      ".aliases".source = ./bash/aliases;
      ".aspell.conf".text = ''
data-dir ${config.home.homeDirectory}/.nix-profile/lib/aspell
master en_US
extra-dicts en-computers.rws
add-extra-dicts en_US-science.rws
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
      ".tmux.conf.osx".source = ./tmux/tmux.conf.osx;
    };

    packages = (with pkgs; [
      aspell
      aspellDicts.en
      aspellDicts.en-computers
      aspellDicts.en-science
      autoconf
      bzip2
      coreutils
      curl
      fontconfig
      gitAndTools.gh
      glibcLocales
      gnum4
      go
      htop
      locale
      nix-prefetch-scripts
      nixfmt
      nodejs
      ocamlformat
      openssl
      powerline-fonts
      powerline-go
      python
      ripgrep
      rlwrap
      rnix-lsp
      silver-searcher
      tree
      wget
    ]) ++ (with pkgs.haskellPackages; [
      hlint
      ormolu
    ]) ++ (with pkgs.nodePackages; [
      bash-language-server
    ]) ++ (with pkgs.ocamlPackages; [
      dune_2
      findlib
      merlin
      ocp-indent
      utop
    ]);

    sessionVariables = {
      EDITOR = "vim";
      LANG = "en_US.UTF-8";
      LANGUAGE = "en_US:es_ES:fr_FR:ja_JP:pt_BR:zh_CN";
      LOCALE_ARCHIVE = "${pkgs.glibcLocales.outPath}/lib/locale/locale-archive";
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
      extraConfig = import ./git/config.nix;
      ignores = [ "*~" "*.swp" "\\#*\\#" ".\\#*" "nohup.out" ];
    };

    home-manager = {
      enable = true;
      path = "~/.nixpkgs";
    };

    neovim = {
      enable = true;
      extraConfig = builtins.readFile vim/vimrc;
      plugins = vimPlugins;
    };

    opam.enable = true;

    tmux = {
      enable = true;
      extraConfig = builtins.readFile ./tmux/tmux.conf;
      tmuxinator.enable = true;
      plugins = with pkgs.tmuxPlugins; [
        {
          plugin = continuum;
          extraConfig = "set -g @continuum-restore 'on'";
        }
        nord-tmux
        prefix-highlight
        resurrect
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
      initExtra = builtins.readFile ./zsh/zshrc;
      oh-my-zsh = {
        enable = true;
        plugins = [
          "cabal"
          "cargo"
          "docker"
          "docker-compose"
          "git"
          "mix"
          "npm"
          "ripgrep"
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
