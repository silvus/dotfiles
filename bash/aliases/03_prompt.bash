#!/bin/bash

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Colors
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

# Utility function so we can test for things like .git/.hg without firing up a separate process
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

_vcs_prompt_git() {
	# Branch name
	local STATUS="$(git status 2>&1)"

	if [[ "$STATUS" != *'Not a git repository'* ]]; then
		local GIT_PROMPT=$(__git_ps1 '%s')
		# Defaut color
		local STATE_COLOR="$TXTBLUE"
		# Defaut picto
		local PICTO="✔"

		if [[ "$STATUS" != *'working directory clean'* ]]; then
			# red if need to commit
			local STATE_COLOR="$TXTRED"
			local PICTO="⚡"
		elif [[ "$STATUS" == *'Your branch is ahead'* ]]; then
			# cyan if need to push
			local STATE_COLOR="$TXTCYAN"
			local PICTO="↑"
		fi

		echo "-[${STATE_COLOR}${GIT_PROMPT}${TXTRESET}]-[${STATE_COLOR}${PICTO}${TXTRESET}]"
	fi
}

# TODO : Add this to _prompt_command_function
_vcs_prompt() {
	if [[ -d ".svn" ]]; then
		echo "-[${TXTYELLOW}svn${TXTRESET}]"
	elif _has_parent_dir ".git"; then
		_vcs_prompt_git
	fi
}

# The error code of the last command, if it has failed in some way
_last_command() {
	local error="$?"
	if [[ "$error" != 0 ]]; then
		_LAST_COMMAND="-[${TXTRED}${error}${TXTRESET}]"
	else
		_LAST_COMMAND=""
	fi
}

# Change color if the user hasn't write permissions on the current directory
_right_to_write() {
	if [[ -w "${PWD}" ]]; then
		_COLOR_END="$TXTRESET"
	else
		_COLOR_END="$TXTYELLOW"
	fi
}

_is_root() {
	if [[ "$EUID" -eq 0 ]]; then
		# Root
		_COLOR_USER="$TXTPURPLE"
	else
		# Other user
		_COLOR_USER="$TXTGREEN"
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

# Prompt
# --------------------------------------------------------------------------------------
_prompt_command_function() {
	_last_command # As this get the last returned code, it should be called first
	_prompt_pwd_length
	_is_root
	_right_to_write
}

export PROMPT_COMMAND=_prompt_command_function

export PS1='\n┌─[\[$TXTGREEN\]\D{%T}\[$TXTRESET\]]-[\[$_COLOR_USER\]\u\[$TXTYELLOW\]@\[$TXTGREEN\]\h\[$TXTRESET\]]$(_vcs_prompt)\[$_LAST_COMMAND\]\n└─[\[$TXTBLUE\]\w\[$TXTRESET\]] \[$_COLOR_END\]\$\[$TXTRESET\] '