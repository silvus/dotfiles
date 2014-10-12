
# History
# -----------------------------------------------------------------------------
export HISTSIZE=2000 # Lines in memory
export HISTFILESIZE=10000 # Lines on disk
export HISTCONTROL=ignoreboth # ignoredups + ignorespace

# Navigation
# -----------------------------------------------------------------------------
alias dot="cd $DOTFILES_PATH"
alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../../'
alias ....='cd ../../../'
alias .....='cd ../../../../'
alias ......='cd ../../../../../'

# Misc
# -----------------------------------------------------------------------------
alias l='ls -lhaF --color=always --group-directories-first'
alias resolution='xrandr -q | grep "*" | cut -d " " -f 4'
alias git='LC_ALL=en_US git'
# Searches for duplicate file (size and md5)
alias doublons='find -not -empty -type f -printf "%s\n" | sort -rn | uniq -d | xargs -I{} -n1 find -type f -size {}c -print0 | xargs -0 md5sum | sort | uniq -w32 --all-repeated=separate'
alias process='ps aux | grep'
alias lynx="lynx -cfg=$HOME/.lynx/.lynxrc -lss=$HOME/.lynx/lynx.lss"

# Vim
# -----------------------------------------------------------------------------
export EDITOR=vim
alias vi='vim'
alias vi_sec="vim -u $HOME/.vim/.vimrc_secure"

# Sshrc
# -----------------------------------------------------------------------------
if [[ -e "$HOME/.ssh/config" ]]; then
	complete -o "default" -o "nospace" -W "$(grep "^Host" $HOME/.ssh/config | grep -v "[?*]" | cut -d " " -f2)" sshrc
fi
