# Navigation
# -----------------------------------------------------------------------------
alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../../'
alias ....='cd ../../../'
alias .....='cd ../../../../'
alias ......='cd ../../../../../'

mkcd() { mkdir -p "$@" && cd "$_"; }

tmpfile() { $EDITOR $(mktemp); }
tmpdir() { cd $(mktemp -d); }

# Misc
# -----------------------------------------------------------------------------
alias l='LC_COLLATE=C ls -lhaF --color=auto --group-directories-first'
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
alias hosts='sudo vim /etc/hosts'
alias mountshow='mount |column -t'
alias copy='xclip -selection clipboard'
alias clipboard='copy' # Same as copy alias
alias calculator='bc -l'
alias photo='eom' # Eye of Mate
alias lynx="lynx -cfg=$HOME/.config/lynx/lynxrc -lss=$HOME/.config/lynx/lynx.lss"
alias tmux="tmux -2" # Make 256 colors work in tmux
alias screendual="xrandr --output HDMI-0 --auto --primary --output VGA-0 --auto --above HDMI-0"
alias screensimple="xrandr --output VGA-0 --off --output HDMI-0 --auto --primary"

# Man
# -----------------------------------------------------------------------------
# Don’t clear the screen after quitting a manual page
export MANPAGER="less -X";
# Get colors in manual pages
man() {
	env \
		LESS_TERMCAP_mb=$(tput bold; tput setaf 4) \
		LESS_TERMCAP_md=$(tput setaf 2) \
		LESS_TERMCAP_me=$(tput sgr0) \
		LESS_TERMCAP_so=$(tput setaf 0; tput setab 2) \
		LESS_TERMCAP_se=$(tput rmso; tput sgr0) \
		LESS_TERMCAP_us=$(tput smul; tput bold; tput setaf 7) \
		LESS_TERMCAP_ue=$(tput rmul; tput sgr0) \
		LESS_TERMCAP_mr=$(tput rev) \
		LESS_TERMCAP_mh=$(tput dim) \
		LESS_TERMCAP_ZN=$(tput ssubm) \
		LESS_TERMCAP_ZV=$(tput rsubm) \
		LESS_TERMCAP_ZO=$(tput ssupm) \
		LESS_TERMCAP_ZW=$(tput rsupm) \
		man "$@"
}

# Vim
# -----------------------------------------------------------------------------
alias vi='vim'
alias vim.sec="vim -x -u $HOME/.vim/vimrc_sec"
alias vim.min="vim --cmd 'let load_plugins = 0'"
alias vim.max="vim --cmd 'let load_plugins = 1'"

# Bindings
# -----------------------------------------------------------------------------
# Disable flow control (bind Ctrl+s in vim)
stty -ixon

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

# Stop backward-kill-word on directory delimiter (bind Ctrl+w in bash / zsh)
stty werase undef
