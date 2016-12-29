
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
alias diskfree='df -H'
alias diskusage='ncdu'
alias resolution='xrandr -q | grep "*" | cut -d " " -f 4'
alias distro='cat /etc/issue'
alias shortcuts='bind -P'
alias sudosu="sudo -Es"
# Searches for duplicates files (size and md5)
alias findduplicate='find -not -empty -type f -printf "%s\n" | sort -rn | uniq -d | xargs -I{} -n1 find -type f -size {}c -print0 | xargs -0 md5sum | sort | uniq -w32 --all-repeated=separate'
# Delete all empty directories
alias findanddeleteemptydirectories="find . -type d -empty -delete"
# Move all files from subfolders to current folder
alias findandmovehereallfiles="find . -mindepth 2 -type f -print -exec mv {} . \;"
alias process='ps aux | grep'
alias ports='netstat -tulanp'
alias tcpdump='tcpdump -i eth0'
alias netusage='lsof -P -i -n'
alias firewalllist='sudo iptables -L -n -v --line-numbers'
alias mountshow='mount |column -t'
alias calculator='bc -l'
alias photo='feh --draw-filename --draw-tinted --scale-down'
alias lynx="lynx -cfg=$HOME/.lynx/lynxrc -lss=$HOME/.lynx/lynx.lss"
alias tmux="tmux -2" # Getting 256 colors to work in tmux
alias screendual="xrandr --output HDMI-0 --auto --primary --output VGA-0 --auto --above HDMI-0"
alias screensimple="xrandr --output VGA-0 --off --output HDMI-0 --auto --primary"

# Vim
# -----------------------------------------------------------------------------
alias vi='vim'
alias vim.sec="vim -x -u $HOME/.vim/vimrc_sec"

# Disable flow control (bind Ctrl+s in vim)
stty -ixon

# Bindings
# -----------------------------------------------------------------------------
# Set keyboard repeat delay and rate
if [[ -x "$(which xset)" ]]; then
	# Default: xset r rate 660 25
	xset r rate 300 30
fi

# Disable nbsp character (AltGr + Space: non-breakable space) if X is running
if [[ -x "$(which setxkbmap)" ]]; then
	setxkbmap -option "nbsp:none"
fi

# Stop backward-kill-word on directory delimiter (Ctrl+w)
stty werase undef
bind '\C-w:unix-filename-rubout'
