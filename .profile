# Aliases
[[ -r "$HOME/.aliases" ]] && source "$HOME/.aliases"

# Functions
[[ -r "$HOME/.functions" ]] && source "$HOME/.functions"

# tmux will set its terminal
[[ -z "$TMUX" ]] && export TERM=xterm-256color

export EDITOR=vim

# A nice online converter https://geoff.greer.fm/lscolors/
export LS_COLORS="di=1;34:ln=1;36:so=1;32:pi=1;33:ex=1;32:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=30;42:ow=34;43"
[[ $(uname -s) = Darwin ]] && export CLICOLOR=1
[[ $(uname -s) = Darwin ]] && export LSCOLORS=ExGxCxDxCxegedabagaced

# Homebrew
PATH=$(sed -e 's#/usr/local/bin:##' <<<$PATH)
export PATH=/usr/local/bin:$PATH

# rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
exists rbenv && eval "$(rbenv init -)"

# Node js
[[ -d /usr/local/lib/node_modules/ ]] && export NODE_PATH=/usr/local/lib/node_modules/

# q
[[ -d "$HOME/q" ]] && export QHOME="$HOME/q"
[[ -d "$QHOME/m32" ]] && export PATH="$PATH:$QHOME/m32"

[[ $(uname -s) = Darwin ]] && setjdk 1.8

# OPAM configuration
[[ -s "$HOME/.opam/opam-init/init.sh" ]] && $HOME/.opam/opam-init/init.sh >/dev/null 2>&1 || true
exists opam && eval $(opam config env)

# Haskell
if [[ -d "$HOME/.cabal/bin" ]]
then
  export PATH="$PATH:$HOME/.cabal/bin"
elif [[ -d "$HOME/Library/Haskell/bin" ]]
then
  export PATH="$PATH:$HOME/Library/Haskell/bin"
fi

export PATH=$HOME/.local/bin:$PATH

# Powerline
[[ -d $(python -m site --user-base)/bin ]] &&
  [[ ! $PATH =~ $(python -m site --user-base)/bin ]] &&
  export PATH="$PATH:$(python -m site --user-base)/bin"
