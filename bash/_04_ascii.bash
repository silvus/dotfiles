
# Get uptime
# --------------------------------------------------------------------
_uptime() {
	local uptime=$(</proc/uptime)
	local uptime=${uptime%%.*}

	local seconds=$(( uptime%60 ))
	local minutes=$(( uptime/60%60 ))
	local hours=$(( uptime/60/60%24 ))
	local days=$(( uptime/60/60/24 ))

	echo "$days days, $hours hours, $minutes minutes, $seconds seconds"
}


# Print ascii art and some system infos
# --------------------------------------------------------------------
asciiart() {
	local OS=$(uname -s)
	local ARCH=$(uname -m)
	local KERNEL=$(uname -r)
	local DATE=$(date)
	local HOST=$(hostname -s)
	local IP_LOCAL=$(hostname  -I | cut -f1 -d' ')
	local UPTIME=$(_uptime)

	# Colors
	local TXTGREEN=$(tput setaf 2)
	local TXTBLUE=$(tput setaf 4)
	local TXTRESET=$(tput sgr0)

	echo -e "$TXTGREEN"'      .--.      '"${USER^} - ${HOST^} - $IP_LOCAL $TXTRESET"
	echo -e "$TXTGREEN"'     |o_o |     '"OS $ARCH $KERNEL $TXTRESET"
	echo -e "$TXTGREEN"'     |:_/ |     '
	echo -e "$TXTGREEN"'    //   \ \    '"Uptime $UPTIME $TXTRESET"
	echo -e "$TXTGREEN"'   (|     | )   '
	echo -e "$TXTGREEN"'  / \_   _/ \   '
	echo -e "$TXTGREEN"'  \___)=(___/   '"${DATE^}$TXTRESET"
}

asciiart
