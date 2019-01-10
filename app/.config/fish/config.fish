
if status --is-interactive
	# Disable flow control (bind Ctrl+s in vim)
	stty -ixon
	# Stop backward-kill-word on directory delimiter (bind Ctrl+w in bash / zsh)
	stty werase undef
end

# Source bash aliases
source ~/.dotfiles/shell/aliases/20_main.sh

# Differents alias
alias l='env LC_COLLATE=C ls -lhaF --color=auto --group-directories-first'
