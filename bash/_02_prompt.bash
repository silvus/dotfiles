#!/bin/bash

# Colors
black=$(tput -Txterm setaf 0)
red=$(tput -Txterm setaf 1)
green=$(tput -Txterm setaf 2)
yellow=$(tput -Txterm setaf 3)
blue=$(tput -Txterm setaf 4)
pink=$(tput -Txterm setaf 5)
cyan=$(tput -Txterm setaf 6)

# Styles
bold=$(tput -Txterm bold)
reset=$(tput -Txterm sgr0)

# Terminal size
terminal_width=$(tput cols)
# terminal_height=$(tput lines)

# TODO : Do something more clean with this
# https://github.com/taringamberini/vcs-bash-prompt
#_prompt_git() {
#    $(git rev-parse --is-inside-git-dir 2>/dev/null ) \
#        && return 1
#    $(git rev-parse --is-inside-work-tree 2>/dev/null ) \
#        || return 1
#    git status &>/dev/null
#    branch=$(git symbolic-ref --quiet HEAD 2>/dev/null ) \
#        || branch=$(git rev-parse --short HEAD 2>/dev/null ) \
#        || branch='unknown'
#    branch=${branch##*/}
#    git diff --quiet --ignore-submodules --cached \
#        || state=${state}+
#    git diff-files --quiet --ignore-submodules -- \
#        || state=${state}!
#    $(git rev-parse --verify refs/stash &>/dev/null ) \
#        && state=${state}^
#    [ -n "$(git ls-files --others --exclude-standard )" ] \
#        && state=${state}?
#    printf '[%s]' "${branch:-unknown}${state}"
#}

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

_end_prompt() {
    if [[ -d ".svn" ]]; then
        echo "${yellow}-[svn]${reset}"
    elif _has_parent_dir ".git"; then
        _vcs_prompt_git
    else
        echo "${reset}\$"
    fi
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

        if [[ 120 -ge "$terminal_width" ]] && [[ "$terminal_width" != 80 ]]; then
            # Short version - without branch
            echo "${STATE_COLOR}${PICTO}${reset}"
        else
            echo "${black}-[${STATE_COLOR}${GIT_PROMPT}${black}]${STATE_COLOR}${PICTO}${reset}"
        fi
    fi
}


# Prompt
# --------------------------------------------------------------------------------------
# 120 >= terminal_width (80 ... bug with guake ?)
if [[ 120 -ge "$terminal_width" ]] && [[ "$terminal_width" != 80 ]]; then
    # Short prompt
    PS1='${bold}${black}[${green}\u${yellow}@${green}\h${black}][${pink}\W${black}]$(_end_prompt) '
else
    PS1='${bold}${black}[${blue}\D{%T}${black}]-[${green}\u${yellow}@${green}\h${black}]-[${pink}\w${black}]$(_end_prompt) '
fi

echo "$terminal_width"

export PS1

