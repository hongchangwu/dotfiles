[[ -s "$HOME/.bashrc" ]] && source "$HOME/.bashrc"

# Aliases
[[ -s "$HOME/.aliases" ]] && source "$HOME/.aliases"

# Functions
[[ -s "$HOME/.functions" ]] && source "$HOME/.functions"

# tmux will set its terminal
[[ -z "$TMUX" ]] && export TERM=xterm-256color

prompt
[[ $(uname -s) = Darwin ]] && export CLICOLOR=1
[[ $(uname -s) = Darwin ]] && export LSCOLORS=ExGxCxDxCxegedabagaced

# Homebrew
PATH=$(sed -e 's#/usr/local/bin:##' <<<$PATH)
export PATH=/usr/local/bin:$PATH

# Node js
[[ -d /usr/local/lib/node_modules/ ]] && export NODE_PATH=/usr/local/lib/node_modules/

# q
[[ -d "$HOME/q" ]] && export QHOME="$HOME/q"
[[ -d "$QHOME/m32" ]] && export PATH="$PATH:$QHOME/m32"

[[ $(uname -s) = Darwin ]] && setjdk 1.8

# OPAM configuration
[[ -s "$HOME/.opam/opam-init/init.sh" ]] && $HOME/.opam/opam-init/init.sh >/dev/null 2>&1 || true
exists opam && eval $(opam config env)

# Powerline
[[ -d "$HOME/Library/Python/2.7/bin" ]] && export PATH="$PATH:$HOME/Library/Python/2.7/bin"
[[ -d "$HOME/Library/Python/2.7/lib/python/site-packages/powerline" ]] && export POWERLINE_HOME="$HOME/Library/Python/2.7/lib/python/site-packages/powerline"

export PATH=$HOME/bin:$PATH

# rvm
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
