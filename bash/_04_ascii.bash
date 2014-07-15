
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

# Get pending updates
# --------------------------------------------------------------------
_packageupdate() {
	# Query pending updates.
	local updates=$(/usr/lib/update-notifier/apt-check 2>&1)

	if [ $? -ne 0 ]; then
		echo "Querying pending updates failed"
		exit 1
	fi

	# Check for the case where there are no updates.
	if [ "$updates" = "0;0" ]; then
		echo "All packages are up-to-date"
		exit 0
	fi;

	# Check for pending security updates.
	local pending_security=$( echo "$updates" | cut -d ";" -f 2)
	# Check for pending non-security updates.
	local pending_regular=$( echo "$updates" | cut -d ";" -f 1)

	echo "$pending_security security update(s) - $pending_regular non-security update(s)"
	exit 0
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

	echo -e "$backgreen"'      .--.     '"$green" "${USER^} - ${HOST^} - $IP_LOCAL"
	echo -e "$backgreen"'     |o_o |    '"$green" "$OS $ARCH $KERNEL"
	echo -e "$backgreen"'     |:_/ |    '
	echo -e "$backgreen"'    //   \ \   '"$green" "Uptime $UPTIME"
	echo -e "$backgreen"'   (|     | )  '
	echo -e "$backgreen"'  / \_   _/ \  '
	echo -e "$backgreen"'  \___)=(___/  '"$green" "${DATE^}"
	echo -e  "$reset"
}

asciiart
