_TXTCOLOR_GREEN=$(tput setaf 2)
_TXTCOLOR_YELLOW=$(tput setaf 3)
_TXTCOLOR_RESET=$(tput sgr0)

# Function for backup a file and make a symlink
# --------------------------------------------------------
make_symlink() {
	local file_name="$1"
	local dot_file_path="$2"
	local file_path="$3"

	# With sudo ?
	if [[ "$4" == "sudo" ]]; then
		local with_sudo="$4"
	else
		local with_sudo=""
	fi

	if [[ ! -d "$BACKUP_DIR" ]]; then
		echo "Backup directory $BACKUP_DIR doesn\'t exist"
		exit 1
	fi

	#  If file_path is not already a symlink or doesn't exist
	if [[ ! -L "$file_path" ]]; then

		# File or directory already exist, make backup
		if [[ -f "$file_path" ]] || [[ -d "$file_path" ]]; then
			echo "${_TXTCOLOR_YELLOW}Backup current $file_name in $BACKUP_DIR/$file_name.bak${_TXTCOLOR_RESET}"
			"$with_sudo" mv "$file_path" "$BACKUP_DIR/$file_name.bak"
		fi

		# Make symlink
		"$with_sudo" ln -sv "$dot_file_path" "$file_path"
	else
		echo "${_TXTCOLOR_GREEN}$file_name is already installed${_TXTCOLOR_RESET}"
	fi
}

# Clone if doesn't exist, else update
# --------------------------------------------------------
clone_or_update() {
	local git_url="$1"
	local install_path="$2"
	local depot_name="$3"

	if [[ -d "$install_path" ]]; then
		# Update
		echo "${_TXTCOLOR_GREEN}Update $depot_name${_TXTCOLOR_RESET}"
		( cd "$install_path" && git pull )
	else
		# Clone
		echo "${_TXTCOLOR_YELLOW}Install $depot_name${_TXTCOLOR_RESET}"
		git clone "$git_url" "$install_path"
	fi
}

# Create directory if necessary
# --------------------------------------------------------
dir_check() {
	local dir_path="$1"
	if [[ ! -d "$dir_path" ]]; then
		mkdir -p "$dir_path"
	fi
}