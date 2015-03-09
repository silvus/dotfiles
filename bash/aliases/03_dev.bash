
# Dotfiles
alias dot="cd $DOTFILES_PATH"

# Projects root folder (and for Virtualenvwrapper)
export PROJECT_HOME="/data/dev"

# Doc folder
export DOCUMENTATION_HOME="/data/doc"

# Launch simple web server
alias server_php="php -sS localhost:8000"
alias server_python="python3 -m http.server 8000"

# Go to projects folder and git pull
# Must be here due to "cd" subshell usage
# ------------------------------------------------------------
dev() {
	local projectname="$1"

	# Check in GOPATH
	if [[ -d "${GOPATH}/src/${projectname}" ]]; then
		cd "${GOPATH}/src/${projectname}"
		# Update project
		if [[ -d ".git" ]]; then
			git pull origin master
		fi
	# go to GOPATH
	else
		cd "${GOPATH}/src"
	fi
	vim -u "${HOME}/.vim/.vimrc_ide"
}
