# Navigation
# -----------------------------------------------------------------------------
alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../../'
alias ....='cd ../../../'
alias .....='cd ../../../../'
alias ......='cd ../../../../../'

alias dotreload="source ${HOME}/.bashrc"
alias dot="cd $SILVUSDOTFILES"
alias d="cd $SILVUSDOC"
alias t="cd $SILVUSTOOLS"
alias s="cd $SILVUSHOME"
alias m="cd $SILVUSMEDIA"

mkcd() { mkdir -p "$@" && cd "$_"; }

tmpfile() { $EDITOR $(mktemp); }
tmpdir() { cd $(mktemp -d); }

# from http://news.ycombinator.com/item?id=4492682
alias tree1="tree --dirsfirst -ChFLQ 1"
alias tree2="tree --dirsfirst -ChFLQ 2"
alias tree3="tree --dirsfirst -ChFLQ 3"
alias tree4="tree --dirsfirst -ChFLQ 4"
alias tree5="tree --dirsfirst -ChFLQ 5"
alias tree6="tree --dirsfirst -ChFLQ 6"

# Misc
# -----------------------------------------------------------------------------
alias l='LC_COLLATE=C ls -lhaF --color=auto --group-directories-first'
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
alias clipboard='copy' # Same as copy alias
alias calculator='bc -l'
alias photo='eom' # Eye of Mate
alias lynx="lynx -cfg=$HOME/.config/lynx/lynxrc -lss=$HOME/.config/lynx/lynx.lss"
alias tmux="tmux -2" # Make 256 colors work in tmux

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
alias keymapazerty="setxkbmap -model pc105 -layout fr,us -variant oss"
alias keymapqwerty="setxkbmap -model pc105 -layout us,fr -variant oss"

# Set keyboard repeat delay and rate
if [[ -x "$(which xset)" ]]; then
	# If X is running
	if xset q &>/dev/null; then
		# Default: xset r rate 660 25
		xset r rate 300 30

		# Disable nbsp character (AltGr + Space: non-breakable space) if X is running
		if [[ -x "$(which setxkbmap)" ]]; then
			setxkbmap -option "nbsp:none"
		fi

		# Disable beeps
		xset b off
	fi
fi

# Disable flow control (bind Ctrl+s in vim)
stty -ixon
# Stop backward-kill-word on directory delimiter (bind Ctrl+w in bash / zsh)
stty werase undef
