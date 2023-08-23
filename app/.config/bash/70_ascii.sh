
# Print ascii art and some system infos
# --------------------------------------------------------------------
__asciiart() {
	local OS=$(uname -s)
	local ARCH=$(uname -m)
	local KERNEL=$(uname -r)
	local HOST=$(hostname -s)
	local IP_LOCAL=$(hostname  -I | cut -f1 -d' ')
	local SHELL=$(ps -p $$ | awk '$1 != "PID" {print $(NF)}' | tr -d '()')
	local UPTIME=$(uptime -p | sed 's/[^ _-]*/\u&/g')
	local DISKSPACE=$(df --output=pcent / | sed -nr '/[[:digit:]]/{s/[[:space:]]+([[:digit:]]+)%/\1/;p}')

	# Colors
	local TXTRED=$(tput setaf 1)
	local TXTGREEN=$(tput setaf 2)
	local TXTBLUE=$(tput setaf 4)
	local TXTYELLOW=$(tput setaf 3)
	local TXTBLUE=$(tput setaf 4)
	local TXTPURPLE=$(tput setaf 5)
	local TXTCYAN=$(tput setaf 6)
	local TXTRESET=$(tput sgr0)

	echo -e "$TXTBLUE"'      .--.      '"${TXTCYAN}${USER}${TXTRESET}@${TXTYELLOW}${HOST}${TXTRESET}"
	echo -e "$TXTBLUE"'     |o_o |     '"${TXTCYAN}IP      ${TXTGREEN}$IP_LOCAL${TXTRESET}"
	echo -e "$TXTBLUE"'     |:_/ |     '"${TXTCYAN}Kernel  ${TXTGREEN}$ARCH $KERNEL${TXTRESET}"
	echo -e "$TXTBLUE"'    //   \ \    '"${TXTCYAN}Shell   ${TXTGREEN}$SHELL${TXTRESET}"
	echo -e "$TXTBLUE"'   (|     | )   '"${TXTCYAN}Disk /  ${TXTGREEN}$DISKSPACE%${TXTRESET}"
	echo -e "$TXTBLUE"'  / \_   _/ \   '"${TXTRESET}"
	echo -e "$TXTBLUE"'  \___)=(___/   '"${TXTCYAN}Uptime  ${TXTGREEN}$UPTIME${TXTRESET}"
	echo ''
}

__asciiart
