# X auto start
# -----------------------------------------------------------------------------
export CUSTOM_AUTO_START_X=1

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

# Programs
# -----------------------------------------------------------------------------
# Editor
export EDITOR=vim

# Browser
if [[ -n "$DISPLAY" ]]; then
    export BROWSER=firefox
else
    export BROWSER=elinks
fi

# Customs paths
# -----------------------------------------------------------------------------
# Dotfiles path (if not set)
export SILVUSDOTFILES=${SILVUSDOTFILES:="$HOME/.dotfiles"}
export SILVUSDOTFILES_CUSTOM=${SILVUSDOTFILES_CUSTOM:="${SILVUSDOTFILES}_custom"}

# Projects root folder (for Virtualenvwrapper, etc...)
export SILVUSPROJECT="/data/dev"

# Dotfiles
export SILVUSDOC="/data/doc"
export SILVUSMEDIA="/data/media"

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
export GPODDER_HOME="$XDG_CONFIG_HOME"/gpodder

# Atom
export ATOM_HOME="$XDG_DATA_HOME"/atom

# History
# -----------------------------------------------------------------------------
export HISTSIZE=65536 # Lines in memory
export HISTFILESIZE=65536 # Lines on disk
export HISTCONTROL=ignoreboth # ignoredups + ignorespace
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

# Environment specific configuration
# ------------------------------------------------------
if [[ -f "$SILVUSDOTFILES_CUSTOM/env.sh" ]]; then
	source "$SILVUSDOTFILES_CUSTOM/env.sh"
fi
