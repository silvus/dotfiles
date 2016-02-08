
# Dotfiles
alias dot="cd $DOTFILES_PATH"
alias dotbin="cd $DOTFILES_PATH/bin"
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

if [[ -d "${PROJECT_HOME}/bin" ]]; then
	export PATH="$PATH:$PROJECT_HOME/bin"
fi

# Golang
# ------------------------------------------------------
export GOPATH="$PROJECT_HOME"

if [[ -d "/usr/local/go/bin" ]]; then
	export PATH="$PATH:/usr/local/go/bin"
fi

# Python
# ------------------------------------------------------
export PYTHONSTARTUP="$HOME/.pythonrc"
export PYTHONPATH="./.venv:$PYTHONPATH"

if [[ -d "${HOME}/.local/bin" ]]; then
	export PATH="$HOME/.local/bin:$PATH"
fi

# Pip
export PIP_DOWNLOAD_CACHE="$HOME/.pip/cache"
export PIP_REQUIRE_VIRTUALENV=true
pipglobal() {
   PIP_REQUIRE_VIRTUALENV="" pip3 "$@"
}
pipvendor() {
   PIP_REQUIRE_VIRTUALENV="" pip3 "$@" -t ".venv"
}

# Virtualenv
export VIRTUAL_ENV_DISABLE_PROMPT=true # Don't touch my PS1
alias activate="source .venv/bin/activate"
alias pythonvenv=".venv/bin/python3"
alias pythonvenvcreate="python3 -m venv --without-pip .venv; source .venv/bin/activate; curl https://bootstrap.pypa.io/get-pip.py | python3; deactivate; source .venv/bin/activate"
alias pythonvenvpip=".venv/bin/pip3"

# Pipsi
export PIPSI_BIN_DIR="$HOME/bin"

# Go to projects folder and git pull
# Must be here due to "cd" subshell usage
# ------------------------------------------------------------
dev() {
	local projectname="$1"

	# Search in projects
	if [[ ! -z "$projectname" ]] && [[ -d "${PROJECT_HOME}/${projectname}" ]]; then
		cd "${PROJECT_HOME}/${projectname}"
		# Update project
		if [[ -d ".git" ]]; then
			git pull origin master
		fi
		# Active virtualenv
		if [[ -f ".venv/bin/activate" ]]; then
			source ".venv/bin/activate"
		fi
		# Open in vim
		vim
	# go to PROJECT_HOME
	else
		cd "${PROJECT_HOME}"
	fi
}
