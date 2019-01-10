
# if status --is-interactive
# end

alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../../'
alias ....='cd ../../../'
alias .....='cd ../../../../'
alias ......='cd ../../../../../'

alias l='LC_COLLATE=C ls -lhaF --color=auto --group-directories-first'
alias vi='vim'
alias diskusage='ncdu'
alias calculator='bc -l'
alias resolution='xrandr -q | grep "*" | cut -d " " -f 4'
alias keymapazerty="setxkbmap -model pc105 -layout fr,us -variant oss"
alias keymapqwerty="setxkbmap -model pc105 -layout us,fr -variant oss"
