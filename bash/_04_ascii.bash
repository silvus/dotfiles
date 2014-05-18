
# Get uptime
# --------------------------------------------------------------------
_uptime() {
	uptime=$(</proc/uptime)
	uptime=${uptime%%.*}

	seconds=$(( uptime%60 ))
	minutes=$(( uptime/60%60 ))
	hours=$(( uptime/60/60%24 ))
	days=$(( uptime/60/60/24 ))

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
	local UPDATE=$(_packageupdate)

	# Colors
	local green=$(tput -Txterm setaf 2)
	local blue=$(tput -Txterm setaf 4)
	local reset=$(tput -Txterm sgr0)

	echo -e "$blue"'     .--.     '"$green" "${USER^} - ${HOST^} - $IP_LOCAL"
	echo -e "$blue"'    |o_o |    '"$green" "$OS $ARCH $KERNEL"
	echo -e "$blue"'    |:_/ |    '"$green" "Uptime $UPTIME"
	echo -e "$blue"'   //   \ \   '
	echo -e "$blue"'  (|     | )  '"$green" "$UPDATE"
	echo -e "$blue"' / \_   _/ \  '
	echo -e "$blue"' \___)=(___/  '"$green" "${DATE^}"
	echo -e  "$reset"
}

asciiart
