
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
	local green=$(tput -Txterm setaf 2;tput setab 0)
	local backgreen=$(tput -Txterm setaf 0;tput setab 2)
	local reset=$(tput -Txterm sgr0)

	echo -e "$backgreen"'      .--.     '"$green ${USER^} - ${HOST^} - $IP_LOCAL $reset"
	echo -e "$backgreen"'     |o_o |    '"$green $OS $ARCH $KERNEL $reset"
	echo -e "$backgreen"'     |:_/ |    '"$reset"
	echo -e "$backgreen"'    //   \ \   '"$green Uptime $UPTIME $reset"
	echo -e "$backgreen"'   (|     | )  '"$reset"
	echo -e "$backgreen"'  / \_   _/ \  '"$reset"
	echo -e "$backgreen"'  \___)=(___/  '"$green ${DATE^}"
	echo -e  "$reset"
}

asciiart
