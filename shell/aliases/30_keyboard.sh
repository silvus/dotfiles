
# Disable flow control (bind Ctrl+s in vim)
stty -ixon
# Stop backward-kill-word on directory delimiter (bind Ctrl+w in bash / zsh)
stty werase undef

# Set keyboard repeat delay and rate
if [[ -x "$(which xset)" ]]; then
	# If X is running
	if xset q &>/dev/null; then
		# Default: xset r rate 660 25
		xset r rate 200 30

		# Disable beeps
		xset b off
	fi
fi
