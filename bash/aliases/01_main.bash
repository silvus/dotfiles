
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
alias l='LC_ALL=C ls -lhaF --color=auto --group-directories-first'
alias rm='rm -I' # prompt if deleting more than 3 files at a time
alias o='xdg-open'
alias diskfree='df -H'
alias diskusage='ncdu'
alias resolution='xrandr -q | grep "*" | cut -d " " -f 4'
alias distro='cat /etc/issue'
alias shortcuts='bind -P'
alias git='LC_ALL=en_US git'
alias sudosu="sudo -Es"
# Searches for duplicates files (size and md5)
alias duplicate='find -not -empty -type f -printf "%s\n" | sort -rn | uniq -d | xargs -I{} -n1 find -type f -size {}c -print0 | xargs -0 md5sum | sort | uniq -w32 --all-repeated=separate'
alias process='ps aux | grep'
alias ports='netstat -tulanp'
alias tcpdump='tcpdump -i eth0'
alias internetusage='lsof -P -i -n'
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
alias vim.sec="vim -x -u $HOME/.vim/vimrc_sec"

stty -ixon # Disable flow control (bind Ctrl+s in vim)
