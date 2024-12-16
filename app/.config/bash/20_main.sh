# Navigation
# -----------------------------------------------------------------------------
alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../../'
alias ....='cd ../../../'
alias .....='cd ../../../../'
alias ......='cd ../../../../../'

alias dot="cd $SILVUSDOTFILES"

# from http://news.ycombinator.com/item?id=4492682
alias tree1="tree --dirsfirst -ChFLQ 1"
alias tree2="tree --dirsfirst -ChFLQ 2"
alias tree3="tree --dirsfirst -ChFLQ 3"
alias tree4="tree --dirsfirst -ChFLQ 4"
alias tree5="tree --dirsfirst -ChFLQ 5"
alias tree6="tree --dirsfirst -ChFLQ 6"

# Misc
# -----------------------------------------------------------------------------
alias l='LC_COLLATE=C ls -lhaFN --color=auto --group-directories-first'
# alias rm='rm -I' # prompt if deleting more than 3 files at a time
alias distro='cat /etc/issue'
alias shortcuts='bind -P'
alias sudosu="sudo -Es"

# Vim
# -----------------------------------------------------------------------------
alias vi='vim'
alias vim.sec="vim -x -u $HOME/.vim/vimrc_sec"
alias vim.min="vim --cmd 'let load_plugins = 0'"
alias vim.max="vim --cmd 'let load_plugins = 1'"
alias v="vim -u $HOME/.vim/vimrc_v"

# Find
# -----------------------------------------------------------------------------
# Searches for duplicates files (size and md5)
alias findduplicate='find -not -empty -type f -printf "%s\n" | sort -rn | uniq -d | xargs -I{} -n1 find -type f -size {}c -print0 | xargs -0 md5sum | sort | uniq -w32 --all-repeated=separate'
# Delete all empty directories
alias findanddeleteemptydirectories="find . -type d -empty -delete"
# Move all files from subfolders to current folder
alias findandmovehereallfiles="find . -mindepth 2 -type f -print -exec mv {} . \;"

# Programs
# -----------------------------------------------------------------------------
alias diskusage='ncdu'
alias copy='xclip -selection clipboard'
alias copytoclipboard='copy' # Same as copy alias
alias calculator='bc -l'
alias photo='eom' # Eye of Mate
alias lynx="lynx -cfg=$HOME/.config/lynx/lynxrc -lss=$HOME/.config/lynx/lynx.lss"
alias bat='batcat'
alias fd='fdfind'
alias lz='lazygit'

# Enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Screen
# -----------------------------------------------------------------------------
alias resolution='xrandr -q | grep "*" | cut -d " " -f 4'
alias screendual="xrandr --output HDMI-0 --auto --primary --output VGA-0 --auto --above HDMI-0"
alias screensimple="xrandr --output VGA-0 --off --output HDMI-0 --auto --primary"

# System
# -----------------------------------------------------------------------------
alias diskfree='df -H'
alias process='ps aux | grep'
alias ports='netstat -tulanp'
alias netusage='lsof -P -i -n'
alias firewalllist='sudo iptables -L -n -v --line-numbers'
alias mountshow='mount |column -t'

# Keymap
# -----------------------------------------------------------------------------
alias keymapazerty="setxkbmap -model pc105 -layout fr,gb -variant oss,intl -option \"grp:shift_caps_toggle,grp_led:scroll,nbsp:level4,lv3:ralt_switch,compose:menu,eurosign:e\""

