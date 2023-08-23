
# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Styles
# -------------------------------------------------------------------------------------------

# TXTBLACK=$(tput setaf 0)
TXTRED=$(tput setaf 1)
TXTGREEN=$(tput setaf 2)
TXTYELLOW=$(tput setaf 3)
TXTBLUE=$(tput setaf 4)
TXTPURPLE=$(tput setaf 5)
TXTCYAN=$(tput setaf 6)

# Styles
# TXTBOLD=$(tput bold)
TXTRESET=$(tput sgr0)

# Utility functions
# -------------------------------------------------------------------------------------------
# Change host color if connect by ssh
_is_ssh() {
	if [[ -n "$SSH_CLIENT" ]] || [[ -n "$SSH_TTY" ]]; then
		_COLOR_HOST="$TXTRED"
	else
		_COLOR_HOST="$TXTGREEN"
	fi
}

# Change color if the user is root
_is_root() {
	if [[ "$EUID" -eq 0 ]]; then
		# Root
		_COLOR_USER="$TXTPURPLE"
	else
		# Other user
		_COLOR_USER="$TXTGREEN"
	fi
}

# Test for things like .git/.hg without firing up a separate process
_has_parent_dir() {
	test -d "$1" && return 0;

	local current="."
	while [[ ! "$current" -ef "$current/.." ]]; do
		if [[ -d "$current/$1" ]]; then
			return 0;
		fi
		local current="$current/..";
	done

	return 1;
}

# Prompt Command functions
# -------------------------------------------------------------------------------------------

# Versioning
_vcs_prompt() {
	if _has_parent_dir ".svn" ; then
		_VCS_PROMPT="-[${TXTYELLOW}svn${TXTRESET}]"
	elif _has_parent_dir ".git"; then
		_VCS_PROMPT="-[${TXTYELLOW}git${TXTRESET}]"
	else
		_VCS_PROMPT=""
	fi
}

# The error code of the last command, if it has failed in some way
_last_command() {
	local error="${?}"
	if [[ "$error" != 0 ]]; then
		_LAST_COMMAND="-[${TXTRED}${error}${TXTRESET}]"
	else
		_LAST_COMMAND=""
	fi
}

# Change color if the user hasn't write permissions on the current directory
_is_writable() {
	if [[ -w "${PWD}" ]]; then
		_COLOR_END="$TXTRESET"
	else
		_COLOR_END="$TXTYELLOW"
	fi
}

# Set directory length
_prompt_pwd_length() {
	# If small terminal : last directory only
	if [[ $(tput cols) -lt 100 ]]; then
		PROMPT_DIRTRIM=1
	elif [[ $(tput cols) -lt 130 ]]; then
		PROMPT_DIRTRIM=2
	else
		# Full path
		PROMPT_DIRTRIM=0
	fi
}

# Check if python virtualenv is active
_is_virtualenv() {
	if [[ ! -z "$VIRTUAL_ENV" ]]; then
		_PYTHON_VENV="[${TXTPURPLE}$(basename "$(dirname "${VIRTUAL_ENV}")")${TXTRESET}]"
	else
		_PYTHON_VENV=""
	fi
}

# Prompt
# --------------------------------------------------------------------------------------
_prompt_command_function() {
	_last_command # As this get the last returned code, it should be called first
	history -a # Bash history handling with multiple terminals
	_prompt_pwd_length
	_is_writable
	_vcs_prompt
	_is_virtualenv
}

PROMPT_COMMAND=_prompt_command_function

# Things that doesn't change during sessions
_is_ssh
_is_root


# ┌─[21:55:59]-[silvus@mars]-[git]
# └─[~/.dotfiles] $
PS1='\n┌─[\[$TXTGREEN\]\D{%T}\[$TXTRESET\]]-[\[$_COLOR_USER\]\u\[$TXTYELLOW\]@\[$_COLOR_HOST\]\h\[$TXTRESET\]]\[$_VCS_PROMPT\]\[$_PYTHON_VENV\]\[$_LAST_COMMAND\]\n└─[\[$TXTBLUE\]\w\[$TXTRESET\]] \[$_COLOR_END\]\$\[$TXTRESET\] '

# Broken when navigating history because of empty variables ($_VCS_PROMPT)
# [21:56:13]-[silvus@mars]-[~/.dotfiles]-[git] $
# PS1='[\[$TXTGREEN\]\D{%T}\[$TXTRESET\]]-[\[$_COLOR_USER\]\u\[$TXTYELLOW\]@\[$_COLOR_HOST\]\h\[$TXTRESET\]]-[\[$TXTBLUE\]\w\[$TXTRESET\]]\[$_VCS_PROMPT\]\[$_PYTHON_VENV\]\[$_LAST_COMMAND\] \[$_COLOR_END\]\$\[$TXTRESET\] '

# -n : unexport PS1 so sub-processes will not inherit it (Fix for /bin/sh)
export -n PS1
export -n PROMPT_COMMAND
export -n PROMPT_DIRTRIM
