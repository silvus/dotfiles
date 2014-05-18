_uptime() {
	uptime=$(</proc/uptime)
	uptime=${uptime%%.*}

	seconds=$(( uptime%60 ))
	minutes=$(( uptime/60%60 ))
	hours=$(( uptime/60/60%24 ))
	days=$(( uptime/60/60/24 ))

	echo "$days days, $hours hours, $minutes minutes, $seconds seconds"
}

asciiart() {
	local OS=$(uname -s)
	local ARCH=$(uname -m)
	local KERNEL=$(uname -r)
	local DATE=$(date)
	local UPTIME=$(_uptime)
	local HOST=$(hostname -s)

	echo -e '     .--.     ' "$HOST"
	echo -e '    |o_o |    ' "$OS" "$ARCH" "$KERNEL"
	echo -e '    |:_/ |    '
	echo -e '   //   \ \   '
	echo -e '  (|     | )  ' "Uptime $UPTIME"
	echo -e ' / \_   _/ \  '
	echo -e ' \___)=(___/  ' "$DATE"
}

asciiart
