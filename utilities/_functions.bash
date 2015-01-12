_TXTCOLOR_RED=$(tput setaf 1)
_TXTCOLOR_GREEN=$(tput setaf 2)
_TXTCOLOR_YELLOW=$(tput setaf 3)
_TXTCOLOR_BLUE=$(tput setaf 4)
# _TXTCOLOR_MAGENTA=$(tput setaf 5)
# _TXTCOLOR_CYAN=$(tput setaf 6)
_TXTCOLOR_RESET=$(tput sgr0)

# Echo
# --------------------------------------------------------
echo_header() {
	echo -e "\n ${_TXTCOLOR_BLUE}⇒ $@${_TXTCOLOR_RESET}"
}
echo_success() {
	echo " ${_TXTCOLOR_GREEN}✔${_TXTCOLOR_RESET} $@"
}
echo_info() {
	echo " ${_TXTCOLOR_BLUE}➜${_TXTCOLOR_RESET} $@"
}
echo_warning() {
	echo " ${_TXTCOLOR_YELLOW}⚠${_TXTCOLOR_RESET} $@"
}
echo_error() {
	echo " ${_TXTCOLOR_RED}✖${_TXTCOLOR_RESET} $@"
}

# Function for backup a file and make a symlink
# --------------------------------------------------------
make_symlink() {
	local file_name="$1"
	local dot_file_path="$2"
	local file_path="$3"

	# With sudo ?
	if [[ "$4" == "sudo" ]]; then
		local with_sudo="sudo "
	else
		local with_sudo=""
	fi

	if [[ ! -d "$BACKUP_DIR" ]]; then
		echo_error "Backup directory $BACKUP_DIR doesn\'t exist"
		exit 1
	fi

	# If file_path is not already a symlink or doesn't exist
	if [[ ! -L "$file_path" ]]; then

		# File or directory already exist, make backup
		if [[ -f "$file_path" ]] || [[ -d "$file_path" ]]; then
			echo_warning "Backup current $file_name in $BACKUP_DIR/$file_name"
			${with_sudo} mv "$file_path" "$BACKUP_DIR/$file_name"
		fi

		# Make symlink
		echo_success "Make symlink $dot_file_path -> $file_path"
		${with_sudo} ln -s "$dot_file_path" "$file_path"
	else
		echo_info "$file_name is already installed"
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
		echo_info "Update $depot_name"
		( cd "$install_path" && git pull )
	else
		# Clone
		echo_success "Install $depot_name"
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
