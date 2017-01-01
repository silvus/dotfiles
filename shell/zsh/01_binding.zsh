# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Ctrl + left and Ctrl + right
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word
# bindkey "^[[1;5C" emacs-forward-word
# bindkey "^[[1;5D" emacs-backward-word

# Enable Ctrl-x-e to edit command line
autoload -U edit-command-line
# Emacs style
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line
# Vi style:
# zle -N edit-command-line
# bindkey -M vicmd v edit-command-line