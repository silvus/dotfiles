
# History
# -----------------------------------------------------------------------------
export HISTSIZE=2000 # Lines in memory
export HISTFILESIZE=10000 # Lines on disk
export HISTCONTROL=erasedups # Erase duplicates

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
alias resolution='xrandr -q | grep "*" | cut -d " " -f 4'
alias l='ls -lhaF --color=always'
alias git='LANG=en_US git'
alias gitlog="git log --oneline --graph --decorate"
# Searches for duplicate file (size and md5)
alias doublons='find -not -empty -type f -printf "%s\n" | sort -rn | uniq -d | xargs -I{} -n1 find -type f -size {}c -print0 | xargs -0 md5sum | sort | uniq -w32 --all-repeated=separate'
alias process='ps aux | grep'

# Vim
# -----------------------------------------------------------------------------
export EDITOR=vim
alias vi='vim'
alias vi_sec="vim -u $HOME/.vim/.vimrc_secure"

# Docker
# -----------------------------------------------------------------------------
alias docker="sudo docker.io"
alias dockercleancontainers="sudo docker.io ps -a --no-trunc | grep 'Exit' | awk '{print \$1}' | xargs -L 1 -r sudo docker.io rm" # Remove temporary built images
alias dockercleanimages="sudo docker.io images -a --no-trunc | grep none | awk '{print \$3}' | xargs -L 1 -r sudo docker.io rmi" # Remove Docker containers with Exit status
complete -F _docker docker  # Résult from : complete -p docker

# Tmux autostart... Maybe one day...
# -----------------------------------------------------------------------------
# if which tmux 2>&1 >/dev/null; then
#     # if no session is started, start a new session
#     test -z ${TMUX} && tmux

#     # when quitting tmux, try to attach
#     while test -z ${TMUX}; do
#         tmux attach || break
#     done
# fi

# Pass
# -----------------------------------------------------------------------------
alias pass="vi_sec $DOTFILES_PATH/.pass" # Crypt with :X
pass_generate() {
	local lenght=$1
	[ "$lenght" == "" ] && lenght=16
	# tr -dc A-Za-z0-9_ < /dev/urandom | head -c ${lenght} | xargs
	local password=$(tr -dc A-Za-z0-9_ < /dev/urandom | head -c ${lenght})
	echo "$password"

	if [ -f /usr/bin/xclip ]; then
		echo $password | tr -d '\n' | xclip -i -selection clipboard
		echo "Password copied to clipboard"
	else
		echo "You should install xclip"
	fi
}

# Make a file backup
# -----------------------------------------------------------------------------
save() {
	cp $1 ${1}-`date +%Y%m%d%H%M`.bak ;
}
