
# Navigation
# -----------------------------------------------------------------------------
alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../../'
alias ....='cd ../../../'
alias .....='cd ../../../../'
alias ......='cd ../../../../../'

mkcd() { mkdir -p "$@" && cd "$_"; }

# Misc
# -----------------------------------------------------------------------------
alias l='ls -lhaF --color=auto --group-directories-first'
alias rm='rm -I' # prompt if deleting more than 3 files at a time
alias o='xdg-open'
alias diskfree='df -H'
alias diskusage='du -ch'
alias resolution='xrandr -q | grep "*" | cut -d " " -f 4'
alias git='LC_ALL=en_US git'
alias sudosu="sudo -Es"
# Searches for duplicates files (size and md5)
alias doublons='find -not -empty -type f -printf "%s\n" | sort -rn | uniq -d | xargs -I{} -n1 find -type f -size {}c -print0 | xargs -0 md5sum | sort | uniq -w32 --all-repeated=separate'
alias process='ps aux | grep'
alias ports='netstat -tulanp'
alias tcpdump='tcpdump -i eth0'
alias firewalllist='sudo iptables -L -n -v --line-numbers'
alias mountshow='mount |column -t'
alias calculator='bc -l'
alias calendar='cal -3'
alias lynx="lynx -cfg=$HOME/.lynx/lynxrc -lss=$HOME/.lynx/lynx.lss"
alias music="mocp"
alias irc="weechat"
alias tmux="tmux -2" # Getting 256 colors to work in tmux

# Vim
# -----------------------------------------------------------------------------
alias vi='vim'
alias vig='gvim'
alias vim.sec="vim -x -u $HOME/.vim/vimrc_sec"
alias vim.ide="vim -u $HOME/.vim/vimrc_ide"
alias gvim.ide="gvim -u $HOME/.vim/vimrc_ide"

stty -ixon # Disable flow control (bind Ctrl+s in vim)
