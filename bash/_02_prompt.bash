#!/bin/bash

# Colors
# black=$(tput -Txterm setaf 0)
red=$(tput -Txterm setaf 1)
green=$(tput -Txterm setaf 2)
yellow=$(tput -Txterm setaf 3)
blue=$(tput -Txterm setaf 4)
pink=$(tput -Txterm setaf 5)
cyan=$(tput -Txterm setaf 6)

# Styles
# bold=$(tput -Txterm bold)
reset=$(tput -Txterm sgr0)

# Utility function so we can test for things like .git/.hg without firing up a separate process
_has_parent_dir() {
    test -d "$1" && return 0;

    current="."
    while [[ ! "$current" -ef "$current/.." ]]; do
        if [[ -d "$current/$1" ]]; then
            return 0;
        fi
        current="$current/..";
    done

    return 1;
}

_vcs_prompt_git() {
    # Branch name
    local STATUS="$(git status 2>&1)"

    if [[ "$STATUS" != *'Not a git repository'* ]]; then
        local GIT_PROMPT=$(__git_ps1 '%s')
        # Defaut color
        local STATE_COLOR="$blue"
        # Defaut picto
        local PICTO="✔"

        if [[ "$STATUS" != *'working directory clean'* ]]; then
            # red if need to commit
            STATE_COLOR="$red"
            PICTO="⚡"
        elif [[ "$STATUS" == *'Your branch is ahead'* ]]; then
            # cyan if need to push
            STATE_COLOR="$cyan"
            PICTO="↑"
        fi

        echo "-[${STATE_COLOR}${GIT_PROMPT}${reset}]-[${STATE_COLOR}${PICTO}${reset}]"
    fi
}

_vcs_prompt() {
    if [[ -d ".svn" ]]; then
        echo "-[${yellow}svn${reset}]"
    elif _has_parent_dir ".git"; then
        _vcs_prompt_git
    fi
}

# Prompt
# --------------------------------------------------------------------------------------
PS1='\n┌─[${green}\D{%T}${reset}]-[${green}\u${yellow}@${green}\h${reset}]$(_vcs_prompt)\n└─[${blue}\w${reset}] \$ '
