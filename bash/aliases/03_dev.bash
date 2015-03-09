
# Dotfiles
alias dot="cd $DOTFILES_PATH"

# Projects root folder (and for Virtualenvwrapper)
export PROJECT_HOME="/data/dev"

# Doc folder
export DOCUMENTATION_HOME="/data/doc"

# Launch simple web server
alias server_php="php -sS localhost:5000"
alias server_python="python3 -m http.server 5000"

# Golang
# ------------------------------------------------------
export GOPATH="/data/go"

if [[ -d "/usr/local/go/bin" ]]; then
	export PATH="$PATH:/usr/local/go/bin"
	if [[ -d "${GOPATH}/bin" ]]; then
		export PATH="$PATH:$GOPATH/bin"
	fi
fi

# Go to projects folder and git pull
# Must be here due to "cd" subshell usage
# ------------------------------------------------------------
dev() {
	local projectname="$1"

	# Check in GOPATH
	if [[ ! -z "$projectname" ]] && [[ -d "${GOPATH}/src/${projectname}" ]]; then
		cd "${GOPATH}/src/${projectname}"
		# Update project
		if [[ -d ".git" ]]; then
			git pull origin master
		fi
		vim -u "${HOME}/.vim/.vimrc_ide"
	# go to GOPATH
	else
		cd "${GOPATH}/src"
	fi
}
