
# Disable flow control (bind Ctrl+s in vim)
stty -ixon
# Stop backward-kill-word on directory delimiter (bind Ctrl+w in bash / zsh)
stty werase undef

# Set keyboard repeat delay and rate
if [[ -x "$(which xset)" ]]; then
	# If X is running
	if xset q &>/dev/null; then
		# Default: xset r rate 660 25
		xset r rate 300 30

		# Disable nbsp character (AltGr + Space: non-breakable space) if X is running
		if [[ -x "$(which setxkbmap)" ]]; then
			setxkbmap -option "nbsp:none"
		fi

		# Disable beeps
		xset b off
	fi
fi
