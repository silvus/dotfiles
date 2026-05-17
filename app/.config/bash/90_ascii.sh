# Print ascii art and system information (portable: Linux + Termux)
# --------------------------------------------------------------------
__asciiart() {

    # System info
    local OS
    OS=$(uname -s 2>/dev/null)

    local ARCH
    ARCH=$(uname -m 2>/dev/null)

    local KERNEL
    KERNEL=$(uname -r 2>/dev/null)

    local HOST
    HOST=$(hostname 2>/dev/null)

    # IP address (Termux-safe fallback chain)
    local IP_LOCAL="N/A"
    if command -v hostname >/dev/null 2>&1; then
        IP_LOCAL=$(hostname -I 2>/dev/null | awk '{print $1}')
    fi
    if [ -z "$IP_LOCAL" ]; then
        if command -v ifconfig >/dev/null 2>&1; then
            IP_LOCAL=$(ifconfig 2>/dev/null |
                awk '/inet / && $2 != "127.0.0.1" {print $2; exit}')
        fi
    fi
    if [ -z "$IP_LOCAL" ] && command -v getprop >/dev/null 2>&1; then
        IP_LOCAL=$(getprop dhcp.wlan0.ipaddress 2>/dev/null)
    fi
    [ "$IP_LOCAL" ] || IP_LOCAL="N/A"

    # Shell detection
    local SHELL
    SHELL=$(ps -p $$ 2>/dev/null | awk 'NR==2 {print $NF}')

    # Uptime (Termux may restrict /proc/uptime)
    local UPTIME="Unavailable"
    if [ -r /proc/uptime ]; then
        UPTIME=$(awk '{
            printf "%d hours, %d minutes",
            $1/3600,
            ($1%3600)/60
        }' /proc/uptime)
    fi

    # Disk usage (no GNU df --output support on Termux)
    local DISKSPACE
    DISKSPACE=$(df / 2>/dev/null | awk 'NR==2 {gsub("%","",$5); print $5}')

    [ "$DISKSPACE" ] || DISKSPACE="N/A"

    # Colors
    local TXTGREEN TXTBLUE TXTYELLOW TXTCYAN TXTRESET

    # TXTRED=$(tput setaf 1)
    TXTGREEN=$(tput setaf 2)
    TXTYELLOW=$(tput setaf 3)
    TXTBLUE=$(tput setaf 4)
    # TXTPURPLE=$(tput setaf 5)
    TXTCYAN=$(tput setaf 6)
    TXTRESET=$(tput sgr0)

    # Output
    echo -e "${TXTBLUE}      .--.      ${TXTRESET}${TXTCYAN}${USER}${TXTRESET}@${TXTYELLOW}${HOST}${TXTRESET}"
    echo -e "${TXTBLUE}     |o_o |     ${TXTRESET}${TXTCYAN}IP      ${TXTGREEN}${IP_LOCAL}${TXTRESET}"
    echo -e "${TXTBLUE}     |:_/ |     ${TXTRESET}${TXTCYAN}Kernel  ${TXTGREEN}${OS} ${ARCH} ${KERNEL}${TXTRESET}"
    echo -e "${TXTBLUE}    //   \ \    ${TXTRESET}${TXTCYAN}Shell   ${TXTGREEN}${SHELL}${TXTRESET}"
    echo -e "${TXTBLUE}   (|     | )   ${TXTRESET}${TXTCYAN}Disk /  ${TXTGREEN}${DISKSPACE}%${TXTRESET}"
    echo -e "${TXTBLUE}  / \_   _/ \   ${TXTRESET}"
    echo -e "${TXTBLUE}  \___)=(___/   ${TXTRESET}${TXTCYAN}Uptime  ${TXTGREEN}${UPTIME}${TXTRESET}"
    echo ''
}

__asciiart
