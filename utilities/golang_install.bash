#!/usr/bin/env bash

if [[ "$1" == "-h" || "$1" == "--help" ]]; then cat <<HELP
Usage: $(basename "$0")

Install golang in 30 seconds with a 'nice' interface.

Require "whiptail"

Copyright (c) 2015 Silvus
Licensed under the MIT license.
HELP
exit; fi


# Requirement
# ------------------------------------------------------------------
check_requirement() {
	if [[ ! -x "$(which whiptail 2>/dev/null)" ]]; then
		echo "This script need whiptail and it's not installed."
		exit 1
	fi
}

# Abort
# ------------------------------------------------------------------
abort_install() {
	local message="$1"
	whiptail --title "Aborting" --msgbox "Error : $message" 8 78
	exit 2
}

# Install path
# ------------------------------------------------------------------
get_go_install_path() {
	OPTION_INSTALL_PATH=$(whiptail --title "Install Go" --inputbox "Install path?" 10 60 "$DEFAULT_INSTALL_PATH" 3>&1 1>&2 2>&3)
	local exitstatus="$?"
	if [[ "$exitstatus" == 0 ]]; then

		# Destination is not a folder : Ask to create it
		if [[ ! -d "$OPTION_INSTALL_PATH" ]]; then
			OPTION_NEED_CREATE_FOLDER=$(whiptail --title "Create base folder" --yesno "$OPTION_INSTALL_PATH is not a directory\n\nCreate it?"  10 70  3>&1 1>&2 2>&3)
			local exitstatus="$?"
			if [[ "$exitstatus" == 0 ]]; then
				NEED_CREATE_FOLDER=1
			else
				abort_install "$OPTION_INSTALL_PATH is not a directory"
			fi
		fi

		# Go is already installed : Ask to delete it
		if [[ -d "${OPTION_INSTALL_PATH}/go" ]]; then
			OPTION_NEED_UNINSTALL=$(whiptail --title "Uninstall previous version" --yesno "${OPTION_INSTALL_PATH}/go already exist\nDelete it?"  10 70  3>&1 1>&2 2>&3)
			local exitstatus="$?"
			if [[ "$exitstatus" == 0 ]]; then
				NEED_UNINSTALL=1
			else
				abort_install "${OPTION_INSTALL_PATH}/go already exist"
			fi
		fi
	else
		exit 1
	fi
}

# Go version
# ------------------------------------------------------------------
get_go_version() {
	# 32-bit or 64-bit ?
	[[ $(uname -m) == 'x86_64' ]] && local ARCH="amd64" || local ARCH="amd32"

	local OS="linux"

	OPTION_GO_VERSION=$(whiptail --title "Go version" --menu "Go Version?" 10 60 4 \
		"1.4.2.${OS}-${ARCH}" "Go 1.4.2" \
		"1.3.3.${OS}-${ARCH}" "Go 1.3.3" 3>&1 1>&2 2>&3)
	local exitstatus="$?"
	if [[ "$exitstatus" != 0 ]]; then
		exit 1
	fi
}

# Confirm
# ------------------------------------------------------------------
get_go_confirm() {
	OPTION_CONFIRM=$(whiptail --title "Confirm" --yesno "It will install ${OPTION_GO_VERSION} to ${OPTION_INSTALL_PATH}/go\n\nContinue?"  10 70  3>&1 1>&2 2>&3)
	local exitstatus="$?"
	if [[ "$exitstatus" != 0 ]]; then
		exit 1
	fi
}

# Install
# ------------------------------------------------------------------
install_go() {

	# Check for 404
	if [[ $(curl -o /dev/null --silent --head --write-out '%{http_code}' "${DOWNLOAD_URL}/go${OPTION_GO_VERSION}.tar.gz") != 200 ]]; then
		abort_install "${DOWNLOAD_URL}/go${OPTION_GO_VERSION}.tar.gz isn't available"
	fi
	# Download
	wget "${DOWNLOAD_URL}/go${OPTION_GO_VERSION}.tar.gz" -P "$TEMP_FOLDER" 2>&1 | \
		stdbuf -o0 awk '/[.] +[0-9][0-9]?[0-9]?%/ { print substr($0,63,3) }' | \
		whiptail --title "Downloading" --gauge "Downloading go${OPTION_GO_VERSION}.tar.gz" 10 70 0

	# Extract
	tar zxf "${TEMP_FOLDER}/go${OPTION_GO_VERSION}.tar.gz" -C "${TEMP_FOLDER}"

	# Uninstall previous version
	if [[ "$NEED_UNINSTALL" == 1 ]]; then
		sudo rm -R "${OPTION_INSTALL_PATH}/go"
	fi

	# Create base folder
	if [[ "$NEED_CREATE_FOLDER" == 1 ]]; then
		sudo mkdir -p "$OPTION_INSTALL_PATH"
	fi

	# Move
	sudo mv "${TEMP_FOLDER}/go" "$OPTION_INSTALL_PATH"

	# Cleanup
	rm -R "${TEMP_FOLDER}"
}

# Report
# ------------------------------------------------------------------
end_install() {
	whiptail --title "Go is installed" --msgbox "Install path: ${OPTION_INSTALL_PATH}/go\nVersion: ${OPTION_GO_VERSION}\n\nYou can add to your \$PATH ${OPTION_INSTALL_PATH}/go/bin" 10 80
	exit 0
}


# Main
# ------------------------------------------------------------------
DOWNLOAD_URL="https://storage.googleapis.com/golang"
DEFAULT_INSTALL_PATH="/usr/local"
NEED_UNINSTALL=0
NEED_CREATE_FOLDER=0
TEMP_FOLDER="/tmp/go"

check_requirement
get_go_install_path
get_go_version
get_go_confirm
install_go
end_install