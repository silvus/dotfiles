#!/bin/bash

# Colors
# TXTBLACK=$(tput -Txterm setaf 0)
TXTRED=$(tput -Txterm setaf 1)
TXTGREEN=$(tput -Txterm setaf 2)
TXTYELLOW=$(tput -Txterm setaf 3)
TXTBLUE=$(tput -Txterm setaf 4)
# TXTPURPLE=$(tput -Txterm setaf 5)
TXTCYAN=$(tput -Txterm setaf 6)

# Styles
# bold=$(tput -Txterm bold)
TXTRESET=$(tput -Txterm sgr0)

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
PS1='\n┌─[${TXTGREEN}\D{%T}${TXTRESET}]-[${TXTGREEN}\u${TXTYELLOW}@${TXTGREEN}\h${TXTRESET}]$(_vcs_prompt)\n└─[${TXTBLUE}\w${TXTRESET}] \$ '
