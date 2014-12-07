
# History
# -----------------------------------------------------------------------------
export HISTSIZE=2000 # Lines in memory
export HISTFILESIZE=10000 # Lines on disk
export HISTCONTROL=ignoreboth # ignoredups + ignorespace

# Navigation
# -----------------------------------------------------------------------------
alias dot="cd $DOTFILES_PATH"
alias dot_up="git --git-dir=$DOTFILES_PATH/.git --work-tree=$DOTFILES_PATH pull origin master && make --no-print-directory -C $DOTFILES_PATH && source $HOME/.bashrc"
alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../../'
alias ....='cd ../../../'
alias .....='cd ../../../../'
alias ......='cd ../../../../../'

mkcd() { mkdir -p "$@" && cd "$_"; }

# Misc
# -----------------------------------------------------------------------------
alias l='ls -lhaF --color=always --group-directories-first'
alias resolution='xrandr -q | grep "*" | cut -d " " -f 4'
alias git='LC_ALL=en_US git'
alias sudosu="sudo -Es"
# Searches for duplicate file (size and md5)
alias doublons='find -not -empty -type f -printf "%s\n" | sort -rn | uniq -d | xargs -I{} -n1 find -type f -size {}c -print0 | xargs -0 md5sum | sort | uniq -w32 --all-repeated=separate'
alias process='ps aux | grep'
alias lynx="lynx -cfg=$HOME/.lynx/.lynxrc -lss=$HOME/.lynx/lynx.lss"
alias music="mocp"
alias tmux="tmux -2" # Getting 256 colors to work in tmux

# Vim
# -----------------------------------------------------------------------------
export EDITOR=vim
alias vi='vim'
alias vi_sec="vim -u $HOME/.vim/.vimrc_secure"

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
