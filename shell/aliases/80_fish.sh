# Auto start fish

# Bash init fish, so .profile are sources and other env variables are sets
# ascii.sh is ignored as fish has his own greeting ascii
if [[ -x "$(which fish)" ]]; then
	# not if already running fish (if bash command in a fish shell)
	# -z: is empty
	if [[ -z "$FISH_IS_STARTED" ]]; then
		exec fish
	fi
fi
