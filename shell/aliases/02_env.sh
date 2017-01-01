
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
export HISTSIZE=10000 # Lines in memory
export HISTFILESIZE=10000 # Lines on disk
export HISTCONTROL=ignoreboth # ignoredups + ignorespace
export HISTTIMEFORMAT="[%F %T] " # timestamps
export HISTIGNORE="jrnl*:jnal*" # Ignore some commands

# Less
# -----------------------------------------------------------------------------
export LESS_TERMCAP_mb=$(tput bold; tput setaf 4)
export LESS_TERMCAP_md=$(tput setaf 2)
export LESS_TERMCAP_me=$(tput sgr0)
export LESS_TERMCAP_so=$(tput setaf 0; tput setab 2)
export LESS_TERMCAP_se=$(tput rmso; tput sgr0)
export LESS_TERMCAP_us=$(tput smul; tput bold; tput setaf 7)
export LESS_TERMCAP_ue=$(tput rmul; tput sgr0)
export LESS_TERMCAP_mr=$(tput rev)
export LESS_TERMCAP_mh=$(tput dim)
export LESS_TERMCAP_ZN=$(tput ssubm)
export LESS_TERMCAP_ZV=$(tput rsubm)
export LESS_TERMCAP_ZO=$(tput ssupm)
export LESS_TERMCAP_ZW=$(tput rsupm)
