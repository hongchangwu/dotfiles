# Source common and interactive profiles
[[ -r "$HOME/.profile" ]] && source "$HOME/.profile"
[[ -r "$HOME/.bashrc" ]] && source "$HOME/.bashrc"

# Aliases
[[ -r "$HOME/.aliases" ]] && source "$HOME/.aliases"

# Functions
[[ -r "$HOME/.functions" ]] && source "$HOME/.functions"

# tmux will set its terminal
[[ -z "$TMUX" ]] && export TERM=xterm-256color

[[ "$TERM" != linux ]] && PROMPT_COMMAND="prompt; $PROMPT_COMMAND"
[[ $(uname -s) = Darwin ]] && export CLICOLOR=1
[[ $(uname -s) = Darwin ]] && export LSCOLORS=ExGxCxDxCxegedabagaced

export EDITOR=vim

# Git auto-completion
[[ -r "$HOME/.git-completion.bash" ]] && source "$HOME/.git-completion.bash"

# Tmux auto-completion
[[ -r "$HOME/.tmux.completion.bash" ]] && source "$HOME/.tmux.completion.bash"

# Tmuxinator auto-completion
[[ -r "$HOME/.tmuxinator.bash" ]] && source "$HOME/.tmuxinator.bash"

# Homebrew
PATH=$(sed -e 's#/usr/local/bin:##' <<<$PATH)
export PATH=/usr/local/bin:$PATH

# rbenv
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
