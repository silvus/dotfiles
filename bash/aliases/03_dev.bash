
# Dotfiles
alias dot="cd $DOTFILES_PATH"
alias dot_reload="source ${HOME}/.bashrc"
alias s="cd /data/silvus"
alias data="cd /data"

# Projects root folder (for Virtualenvwrapper, go, etc...)
export PROJECT_HOME="/data/dev"

# Doc folder
export DOCUMENTATION_HOME="/data/doc"

# Launch simple web server
alias server="python3 -m http.server 5000"
alias server_php="php -sS localhost:5000"

# Golang
# ------------------------------------------------------
export GOPATH="$PROJECT_HOME"

if [[ -d "/usr/local/go/bin" ]]; then
	export PATH="$PATH:/usr/local/go/bin"
	if [[ -d "${GOPATH}/bin" ]]; then
		export PATH="$PATH:$GOPATH/bin"
	fi
fi

# Python
# ------------------------------------------------------
export PYTHONPATH="./.pip:$PYTHONPATH"

if [[ -d "${HOME}/.local/bin" ]]; then
	export PATH="$HOME/.local/bin:$PATH"
fi

# Pip
export PIP_DOWNLOAD_CACHE="$HOME/.pip/cache"
export PIP_REQUIRE_VIRTUALENV=true
pipglobal() {
   PIP_REQUIRE_VIRTUALENV="" pip3 "$@"
}

# Virtualenv
export VIRTUAL_ENV_DISABLE_PROMPT=true # Don't touch my PS1
alias pythonvenvcreate="python3 -m venv --without-pip .venv; source .venv/bin/activate; curl https://bootstrap.pypa.io/get-pip.py | python; deactivate; source .venv/bin/activate"
alias activate="source .venv/bin/activate"

# Go to projects folder and git pull
# Must be here due to "cd" subshell usage
# ------------------------------------------------------------
dev() {
	local projectname="$1"

	# Search in projects
	if [[ ! -z "$projectname" ]] && [[ -d "${PROJECT_HOME}/src/${projectname}" ]]; then
		cd "${PROJECT_HOME}/src/${projectname}"
		# Update project
		if [[ -d ".git" ]]; then
			git pull origin master
		fi
		# Active virtualenv
		if [[ -f ".venv/bin/activate" ]]; then
			source ".venv/bin/activate"
		fi
		# Open in vim
		vim.ide
	# go to PROJECT_HOME
	else
		cd "${PROJECT_HOME}/src"
	fi
}
