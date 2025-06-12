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
/T            \_'$i'--='$m'=='$o')            '$m'IP      '$i(ip -4 -o addr show scope global | awk '{print $4}' | cut -d/ -f1 | head -n1)$o'
'$mouth' \ '$m'('$i$eye$m')   '$o'\~    \_'$i'-='$m'='$o')            '$m'Uptime  '$i(awk '{printf "%d hours, %d minutes\n", $1/3600, ($1%3600)/60}' /proc/uptime)$o'
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

function fish_greeting
	fish_logo
end

