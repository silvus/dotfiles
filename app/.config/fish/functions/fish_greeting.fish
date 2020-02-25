function fish_logo \
    --description="Fish-shell colorful ASCII-art logo" \
    --argument-names outer_color medium_color inner_color mouth eye
    # https://github.com/laughedelic/fish_logo

    # defaults:
    [ $outer_color  ]; or set outer_color  'blue'
    [ $medium_color ]; or set medium_color 'cyan'
    [ $inner_color  ]; or set inner_color  'green'
    [ $mouth ]; or set mouth '['
    [ $eye   ]; or set eye   'O'

    # shortcuts:
    set o (set_color $outer_color)
    set m (set_color $medium_color)
    set i (set_color $inner_color)

    echo '                 '$o'___
  ___======____='$m'-'$i'-'$m'-='$o')                     '$o$USER'@'(hostname -s)$o'
/T            \_'$i'--='$m'=='$o')            '$m'IP      '$i(hostname  -I | cut -f1 -d' ')$o'
'$mouth' \ '$m'('$i$eye$m')   '$o'\~    \_'$i'-='$m'='$o')            '$m'Uptime  '$i(uptime -p | sed 's/[^ _-]*/\u&/g')$o'
 \      / )J'$m'~~    '$o'\\'$i'-='$o')            '$m'Kernel  '$i(uname -s) (uname -m) (uname -r)$o'
  \\\\___/  )JJ'$m'~'$i'~~   '$o'\)             '$m'Disk /  '$i(df --output=pcent  / | sed -nr '/[[:digit:]]/{s/[[:space:]]+([[:digit:]]+)%/\1/;p}')'%'$o'
   \_____/JJJ'$m'~~'$i'~~    '$o'\\            '$m'Date    '$i(date | sed 's/[^ _-]*/\u&/g')$o'
   '$m'/ '$o'\  '$i', \\'$o'J'$m'~~~'$i'~~     '$m'\\
  (-'$i'\)'$o'\='$m'|'$i'\\\\\\'$m'~~'$i'~~       '$m'L_'$i'_
  '$m'('$o'\\'$m'\\)  ('$i'\\'$m'\\\)'$o'_           '$i'\=='$m'__
   '$o'\V    '$m'\\\\'$o'\) =='$m'=_____   '$i'\\\\\\\\'$m'\\\\
          '$o'\V)     \_) '$m'\\\\'$i'\\\\JJ\\'$m'J\)
                      '$o'/'$m'J'$i'\\'$m'J'$o'T\\'$m'JJJ'$o'J)
                      (J'$m'JJ'$o'| \UUU)
                       (UU)'(set_color normal)
end

function cromulon \
    --description "Planet Music audition call"
    # https://github.com/romulof/fish-cromulon
    
    set_color yellow
    echo "        ___"
    echo "    . -^   `--,        ┌─┐┬ ┬┌─┐┬ ┬  ┌┬┐┌─┐"
    echo "   /# =========`-_     └─┐├─┤│ ││││  │││├┤ "
    echo "  /# (--====___====\\   └─┘┴ ┴└─┘└┴┘  ┴ ┴└─┘"
    echo " /#   .- --.  . --.|   ┬ ┬┬ ┬┌─┐┌┬┐  ┬ ┬┌─┐┬ ┬  ┌─┐┌─┐┌┬┐"
    echo "/##   |  * ) (   * ),  │││├─┤├─┤ │   └┬┘│ ││ │  │ ┬│ │ │ "
    echo "|##   \    /\ \   / |  └┴┘┴ ┴┴ ┴ ┴    ┴ └─┘└─┘  └─┘└─┘ ┴ "
    echo "|###   ---   \ ---  |"
    echo "|####      ___)    #|"
    echo  "|######           ##|"
    echo " \\##### ---------- /"
    echo "  \\####           ("
    echo "   `\\###          |"
    echo "     \\###         |"
    echo "      \\##        |"
    echo "       \\###.    .)"
    echo "        `======/"
    echo ""
    set_color normal
end

function fish_greeting
	# cromulon
	fish_logo
end

