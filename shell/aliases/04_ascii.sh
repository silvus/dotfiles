
# Print ascii art and some system infos
# --------------------------------------------------------------------
__asciiart() {
	local OS=$(uname -s)
	local ARCH=$(uname -m)
	local KERNEL=$(uname -r)
	local DATE=$(date)
	local HOST=$(hostname -s)
	local IP_LOCAL=$(hostname  -I | cut -f1 -d' ')
	local SHELL=$(ps -p $$ | awk '$1 != "PID" {print $(NF)}' | tr -d '()')

	# Get uptime
	local uptimevalue=$(</proc/uptime)
	local uptimevalue=${uptimevalue%%.*}
	local seconds=$(( uptimevalue%60 ))
	local minutes=$(( uptimevalue/60%60 ))
	local hours=$(( uptimevalue/60/60%24 ))
	local days=$(( uptimevalue/60/60/24 ))
	local UPTIME="$days days, $hours hours, $minutes minutes, $seconds seconds"

	# Colors
	local TXTGREEN=$(tput setaf 2)
	local TXTBLUE=$(tput setaf 4)
	local TXTRESET=$(tput sgr0)

	echo -e "$TXTBLUE"'      .--.      '"${TXTGREEN}${USER}@${HOST} - $IP_LOCAL$TXTRESET"
	echo -e "$TXTBLUE"'     |o_o |     '"${TXTGREEN}OS $ARCH $KERNEL$TXTRESET"
	echo -e "$TXTBLUE"'     |:_/ |     '"${TXTGREEN}Shell $SHELL$TXTRESET"
	echo -e "$TXTBLUE"'    //   \ \    '"$TXTRESET"
	echo -e "$TXTBLUE"'   (|     | )   '"${TXTGREEN}Uptime $UPTIME$TXTRESET"
	echo -e "$TXTBLUE"'  / \_   _/ \   '"$TXTRESET"
	echo -e "$TXTBLUE"'  \___)=(___/   '"${TXTGREEN}${DATE}$TXTRESET"
	echo ''
}

__asciiart
