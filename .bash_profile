# Source common and interactive profiles
[[ -r "$HOME/.profile" ]] && source "$HOME/.profile"
[[ -r "$HOME/.bashrc" ]] && source "$HOME/.bashrc"

# Completions
[[ -r "$HOME/.local/etc/profile.d/bash_completion.sh" ]] && source "$HOME/.local/etc/profile.d/bash_completion.sh"

[[ "$TERM" != linux ]] && PROMPT_COMMAND="prompt; $PROMPT_COMMAND"

# Git auto-completion
[[ -r "$HOME/.git-completion.bash" ]] && source "$HOME/.git-completion.bash"

# Tmux auto-completion
[[ -r "$HOME/.tmux.completion.bash" ]] && source "$HOME/.tmux.completion.bash"

# Tmuxinator auto-completion
[[ -r "$HOME/.tmuxinator.bash" ]] && source "$HOME/.tmuxinator.bash"
