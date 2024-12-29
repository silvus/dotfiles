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
export SILVUSDOTFILES_CUSTOM="${SILVUSDOTFILES}/custom"


# Customs paths
# -----------------------------------------------------------------------------
# Projects root folder (for Virtualenvwrapper, etc...)
export SILVUSPROJECT="/data/dev"

# Doc
export SILVUSDOC="/data/doc"


# Locales
# -----------------------------------------------------------------------------
export LANGUAGE=en
export LANG=en_US.UTF-8
export LC_NUMERIC=fr_FR.UTF-8
export LC_TIME=fr_FR.UTF-8
export LC_MONETARY=fr_FR.UTF-8
export LC_PAPER=fr_FR.UTF-8
export LC_NAME=fr_FR.UTF-8
export LC_ADDRESS=fr_FR.UTF-8
export LC_TELEPHONE=fr_FR.UTF-8
export LC_MEASUREMENT=fr_FR.UTF-8
export LC_IDENTIFICATION=fr_FR.UTF-8
export PAPERSIZE=a4


# PATH
# ------------------------------------------------------
if [[ -d "${HOME}/bin" ]]; then
	export PATH="${HOME}/bin:$PATH"
fi

if [[ -d "${SILVUSDOTFILES}/bin" ]]; then
	export PATH="${SILVUSDOTFILES}/bin:$PATH"
fi

if [[ -d "${SILVUSDOC}/.bin" ]]; then
	export PATH="${SILVUSDOC}/.bin:$PATH"
fi

if [[ -d "${SILVUSPROJECT}/bin" ]]; then
	export PATH="${SILVUSPROJECT}/bin:$PATH"
fi

if [[ -d "${HOME}/.local/bin" ]]; then
	export PATH="$HOME/.local/bin:$PATH"
fi


# Man
# -----------------------------------------------------------------------------
if [[ -x "$(which most)" ]]; then
	# Color pager
	export MANPAGER="most";
else
	# Don’t clear the screen after quitting a manual page
	export MANPAGER="less -X";
fi


# XDG
# -----------------------------------------------------------------------------
# if not already set
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:="$HOME/.config"}
export XDG_DATA_HOME=${XDG_DATA_HOME:="$HOME/.local/share"}

# Elinks
export ELINKS_CONFDIR="$XDG_CONFIG_HOME"/elinks

# GPG
export GNUPGHOME="$XDG_CONFIG_HOME"/gnupg

# Gpodder
export GPODDER_HOME="$XDG_DATA_HOME"/gpodder


# History
# -----------------------------------------------------------------------------
export HISTSIZE=65536 # Lines in memory
export HISTFILESIZE=65536 # Lines on disk
export HISTCONTROL=ignoreboth # Don't put duplicate lines or lines starting with space in the history.
# export HISTTIMEFORMAT="[%F %T] " # timestamps
# export HISTIGNORE="jrnl*:jnal*" # Ignore some commands


# Fzf
# -----------------------------------------------------------------------------
# export FZF_DEFAULT_COMMAND='pt --hidden --follow --global-gitignore --ignore=".git/" --files-with-matches ""'
# Ignore only hiddens directories
# export FZF_DEFAULT_COMMAND="find . -path '*/\.*' -type d -prune -o -type f -print -o -type l -print 2> /dev/null | sed s/^..//"
# Do not ignore hiddens files (except .git and .svn)
export FZF_DEFAULT_COMMAND="find . -type f -not -path '*/\.git/*' -not -path '*/\.svn/*' -print -o -type l -print 2> /dev/null | sed s/^..//"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"


# Rust
# -----------------------------------------------------------------------------
# Use XDG
export CARGO_HOME="$XDG_DATA_HOME"/cargo
export RUSTUP_HOME="$XDG_DATA_HOME"/rustup
if [[ -d "$CARGO_HOME" ]]; then
	# rustup shell setup ($PATH)
	source "$CARGO_HOME/env"
fi


# Defaults Programs
# -----------------------------------------------------------------------------
# Editor
if [[ -x "$(which nvim)" ]]; then
	export EDITOR="$(which nvim)"
elif [[ -x "$(which vim)" ]]; then
	export EDITOR="$(which vim)"
else
	export EDITOR="$(which vi)"
fi

# Browser
if [[ -n "$DISPLAY" ]]; then
	if [[ -x "$(which firefox)" ]]; then
		export BROWSER="$(which firefox)"
	elif [[ -x "$(which chromium)" ]]; then
		export BROWSER="$(which chromium)"
	fi
elif [[ -x "$(which elinks)" ]]; then
	export BROWSER="$(which elinks)"
elif [[ -x "$(which links)" ]]; then
	export BROWSER="$(which links)"
elif [[ -x "$(which w3m)" ]]; then
	export BROWSER="$(which w3m)"
fi


# X auto start
# -----------------------------------------------------------------------------
export CUSTOM_AUTO_START_X=1


# Nix
# ------------------------------------------------------
if [[ -f "$HOME/.nix-profile/etc/profile.d/nix.sh" ]]; then
	source "$HOME/.nix-profile/etc/profile.d/nix.sh"
fi
# Home Manager
if [[ -f "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" ]]; then
	source "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
fi


# Environment specific configuration
# ------------------------------------------------------
if [[ -f "$SILVUSDOTFILES_CUSTOM/env.sh" ]]; then
	source "$SILVUSDOTFILES_CUSTOM/env.sh"
fi


# Auto start X on tty1
# Only if startx is installed, on first tty and if CUSTOM_AUTO_START_X is set to 1 (default but can be altered in $SILVUSDOTFILES_CUSTOM/env.sh)
# ------------------------------------------------------
if [[ -x "$(which startx)" ]] && [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]] && [[ "$CUSTOM_AUTO_START_X" -eq 1 ]]; then
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
