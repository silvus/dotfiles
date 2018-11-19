
if [[ -f ~/.bash_sandman ]]; then
	source ~/.bash_sandman
elif [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
	exec startx
fi

source ~/.bashrc
