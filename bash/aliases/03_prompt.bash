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

_user_color() {
	# Root
	if [[ "$EUID" -eq 0 ]]; then
		echo "$TXTPURPLE"
	# Other user
	else
		echo "$TXTGREEN"
	fi
}

_prompt_end() {
	if [[ -w "${PWD}" ]]; then
		echo "\$"
    else
    	# if the user hasn't write permissions on the current directory
        echo "${TXTYELLOW}\$${TXTRESET}"
    fi
}

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

_vcs_prompt() {
    if [[ -d ".svn" ]]; then
        echo "-[${TXTYELLOW}svn${TXTRESET}]"
    elif _has_parent_dir ".git"; then
        _vcs_prompt_git
    fi
}

# Prompt
# --------------------------------------------------------------------------------------
PS1='\n┌─[\[$TXTGREEN\]\D{%T}\[$TXTRESET\]]-[$(_user_color)\u\[$TXTYELLOW\]@\[$TXTGREEN\]\h\[$TXTRESET\]]$(_vcs_prompt)\n└─[\[$TXTBLUE\]\w\[$TXTRESET\]] $(_prompt_end) '
