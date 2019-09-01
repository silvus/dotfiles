# Auto start fish

# If interactive shell
if [ -n "$PS1" ]; then
	# Fish is installed
	if [[ -x "$(which fish)" ]]; then
		# not if already running fish (if bash command in a fish shell)
		# -z: is empty
		if [[ -z "$FISH_IS_STARTED" ]]; then
			# Not with "bash -c (command)"
			if [[ -z "$BASH_EXECUTION_STRING" ]]; then
				# Not on a tty (fish is broken on tty ?)
				if [[ ! "$(tty)" =~ /dev/tty ]] ; then
					exec fish
				fi
			fi
		fi
	fi
fi
