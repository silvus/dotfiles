
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
export XDG_CONFIG_HOME="$HOME/.config"

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
# export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
