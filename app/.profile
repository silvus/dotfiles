# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login exists.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# Dotfiles path
# ------------------------------------------------------
_bash_source="${BASH_SOURCE[0]}"
while [ -h "$_bash_source" ]; do # resolve $_bash_source until the file is no longer a symlink
	SILVUSDOTFILES="$( cd -P "$( dirname "$_bash_source" )"/../ && pwd )"
	_bash_source="$(readlink "$_bash_source")"
	[[ $_bash_source != /* ]] && _bash_source="$DIR/$SOURCE" # if $_bash_source was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done

export SILVUSDOTFILES="$( cd -P "$( dirname "$_bash_source" )"/../ && pwd )"
export SILVUSDOTFILES_CUSTOM="${SILVUSDOTFILES}_custom"


# Env variables
# ------------------------------------------------------
if [[ -f "$SILVUSDOTFILES/shell/env.sh" ]]; then
	source "$SILVUSDOTFILES/shell/env.sh"
fi


# Auto start X on tty1
# ------------------------------------------------------
if [[ -x "$(which startx)" ]] && [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
	# Auto start x
	exec startx
fi


# Bashrc
# ------------------------------------------------------
# if running bash
if [ -n "$BASH_VERSION" ]; then
	# include .bashrc if it exists
	if [[ -f "$HOME/.bashrc" ]]; then
		source "$HOME/.bashrc"
	fi
fi
