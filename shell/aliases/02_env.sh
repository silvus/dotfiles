
# Editor
export EDITOR=vim

# Browser
if [[ -n "$DISPLAY" ]]; then
    export BROWSER=firefox
else
    export BROWSER=elinks
fi

# XDG
# -----------------------------------------------------------------------------
# if not already set
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:="$HOME/.config"}
export XDG_DATA_HOME=${XDG_DATA_HOME:="$HOME/.local/share"}

# Elinks
export ELINKS_CONFDIR="$XDG_CONFIG_HOME"/elinks

# History
# -----------------------------------------------------------------------------
export HISTSIZE=4096 # Lines in memory
export HISTFILESIZE=4096 # Lines on disk
export HISTCONTROL=ignoreboth # ignoredups + ignorespace
export HISTTIMEFORMAT="[%F %T] " # timestamps
export HISTIGNORE="jrnl*:jnal*" # Ignore some commands

# Fzf
# -----------------------------------------------------------------------------
# export FZF_DEFAULT_COMMAND='pt --hidden --follow --global-gitignore --ignore=".git/" --files-with-matches ""'
# Ignore only hiddens directories
# export FZF_DEFAULT_COMMAND="find . -path '*/\.*' -type d -prune -o -type f -print -o -type l -print 2> /dev/null | sed s/^..//"
# Do not ignore hiddens files (except .git and .svn)
export FZF_DEFAULT_COMMAND="find . -type f -not -path '*/\.git/*' -not -path '*/\.svn/*' -print -o -type l -print 2> /dev/null | sed s/^..//"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
