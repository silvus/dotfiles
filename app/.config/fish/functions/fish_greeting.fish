function fish_logo \
    --description="Fish-shell colorful ASCII-art logo" \
    --argument-names outer_color medium_color inner_color mouth eye

    # Default colours and symbols
    [ $outer_color ]; or set outer_color blue
    [ $medium_color ]; or set medium_color cyan
    [ $inner_color ]; or set inner_color green
    [ $mouth ]; or set mouth '['
    [ $eye ]; or set eye O

    # Colour shortcuts
    set o (set_color $outer_color)
    set m (set_color $medium_color)
    set i (set_color $inner_color)

    # Hostname
    # hostname -s is not portable on Termux.
    set host_name (hostname 2>/dev/null)
    [ "$host_name" ]; or set host_name unknown

    # IP address detection
    # Termux restricts many iproute2/netlink operations.
    # Use multiple fallbacks.
    set ip_addr ""

    # First attempt: hostname -I
    if command -q hostname
        set ip_addr (
            hostname -I 2>/dev/null | awk '{print $1}'
        )
    end

    # Second attempt: ifconfig (requires net-tools package)
    if test -z "$ip_addr"
        if command -q ifconfig
            set ip_addr (
                ifconfig 2>/dev/null |
                awk '/inet / && $2 != "127.0.0.1" {print $2; exit}'
            )
        end
    end

    # Third attempt: Android system property
    if test -z "$ip_addr"
        if command -q getprop
            set ip_addr (
                getprop dhcp.wlan0.ipaddress 2>/dev/null
            )
        end
    end

    # Final fallback
    [ "$ip_addr" ]; or set ip_addr N/A

    # Uptime
    # Termux devices deny access to /proc/uptime.
    if test -r /proc/uptime
        set uptime_text (
            awk '{
                printf "%d hours, %d minutes",
                $1/3600,
                ($1%3600)/60
            }' /proc/uptime
        )
    else
        set uptime_text Unavailable
    end

    # Disk usage
    # GNU df --output=pcent is unavailable on Toybox/BusyBox.
    set disk_percent (
        df / 2>/dev/null |
        awk 'NR==2 {gsub("%","",$5); print $5}'
    )
    [ "$disk_percent" ]; or set disk_percent N/A

    # Current date
    # Avoid GNU-specific sed formatting.
    set current_date (
        date 2>/dev/null |
        awk '{
            for (i = 1; i <= NF; i++) {
                $i = toupper(substr($i,1,1)) substr($i,2)
            }
            print
        }'
    )

    # Kernel
    set kernel_info (uname -s)" "(uname -m)" "(uname -r)

    # Distro detection
    set DISTRO generic
    if test -r /etc/os-release
        set ID (grep '^ID=' /etc/os-release | cut -d= -f2 | tr -d '"')

        if test "$ID" = nixos
            set DISTRO nixos
        end
    end

    # Render ASCII logo
    if test "$DISTRO" = nixos
        echo "        $o""__    $m""____    __$o                  $m$USER$o@$host_name"
        echo "       $o/  \\   $m\\   \\  /  \\$o         $m""IP      $i$ip_addr$o"
        echo "       $o\\   \\   $m\\   \\/   /$o         $m""Kernel  $i$kernel_info$o"
        echo "     $o""___\\   \\___$m\\      /$o          $m""Disk /  $i$disk_percent%$o"
        echo "    $o/            $m\\    /$o   /\\      $m""Date    $i$current_date$o"
        echo "   $o/______________$m\\   \\$o  /  \\     $m""Uptime  $i$uptime_text$o"
        echo "        $m/   /      \\   \\$o/   /$o"
        echo "$m ______/   /        \\  $o/   /___"
        echo "$m/         /          $o\\/        \\"
        echo "$m\\____    /$o\\          /$m   ______/"
        echo "$m    /   /$o  \\        /$m   /"
        echo "$m""   /   /""$o""\\   \\$m""______""$o""/""$m""___""$o""/""$m""_____"
        echo "$m""   \\  /""$o""  \\   \\$m""              /"
        echo "$m""    \\/""$o""   /    \\$m""____    ____/"
        echo "$o       /      \\$m   \\   \\"
        echo "$o      /   /\\   \\$m   \\   \\"
        echo "$o      \\__/  \\___\\$m   \\__/"
        echo ""

    else
        echo '                 '$o'___
  ___======____='$m'-'$i'-'$m'-='$o')                     '$o$USER'@'$host_name$o'
/T            \_'$i'--='$m'=='$o')            '$m'IP      '$i$ip_addr$o'
'$mouth' \ '$m'('$i$eye$m')   '$o'\~    \_'$i'-='$m'='$o')            '$m'Uptime  '$i$uptime_text$o'
 \      / )J'$m'~~    '$o'\\'$i'-='$o')            '$m'Kernel  '$i$kernel_info$o'
  \\\\___/  )JJ'$m'~'$i'~~   '$o'\)             '$m'Disk /  '$i$disk_percent'%'$o'
   \_____/JJJ'$m'~~'$i'~~    '$o'\\            '$m'Date    '$i$current_date$o'
   '$m'/ '$o'\  '$i', \\'$o'J'$m'~~~'$i'~~     '$m'\\
  (-'$i'\)'$o'\='$m'|'$i'\\\\\\'$m'~~'$i'~~       '$m'L_'$i'_
  '$m'('$o'\\'$m'\\)  ('$i'\\'$m'\\\)'$o'_           '$i'\=='$m'__
   '$o'\V    '$m'\\\\'$o'\) =='$m'=_____   '$i'\\\\\\\\'$m'\\\\
          '$o'\V)     \_) '$m'\\\\'$i'\\\\JJ\\'$m'J\)
                      '$o'/'$m'J'$i'\\'$m'J'$o'T\\'$m'JJJ'$o'J)
                      (J'$m'JJ'$o'| \UUU)
                       (UU)'(set_color normal)
    end
end

# Fish greeting hook
function fish_greeting
    fish_logo
end
