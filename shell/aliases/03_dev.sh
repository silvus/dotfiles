# Projects root folder (for Virtualenvwrapper, go, etc...)
export SILVUSPROJECT="/data/dev"

# Dotfiles
export SILVUSDOC="/data/doc"
export SILVUSTOOLS="/data/tools"
export SILVUSHOME="/data/silvus"

alias dotreload="source ${HOME}/.bashrc"
alias dot="cd $SILVUSDOTFILES"
alias d="cd $SILVUSDOC"
alias t="cd $SILVUSTOOLS"
alias s="cd $SILVUSHOME"

# PATH
# ------------------------------------------------------
if [[ -d "${HOME}/bin" ]]; then
	export PATH="${HOME}/bin:$PATH"
fi

if [[ -d "${SILVUSDOTFILES}/bin" ]]; then
	export PATH="${SILVUSDOTFILES}/bin:$PATH"
fi

if [[ -d "${SILVUSDOC}/.bin" ]]; then
	export PATH="${SILVUSDOC}/.bin:$PATH"
fi
if [[ -d "${SILVUSTOOLS}/.bin" ]]; then
	export PATH="${SILVUSTOOLS}/.bin:$PATH"
fi

if [[ -d "${SILVUSPROJECT}/bin" ]]; then
	export PATH="${SILVUSPROJECT}/bin:$PATH"
fi

# Golang
# ------------------------------------------------------
export GOPATH="$SILVUSPROJECT"

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
pipvendor() {
   pip3 "$@" -t ".venv"
}

# Pipenv
# export PIPENV_VENV_IN_PROJECT=true

# Pew
export WORKON_HOME="/data/dev/.venvs"

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
	if [[ ! -z "$projectname" ]] && [[ -d "${SILVUSPROJECT}/${projectname}" ]]; then
		cd "${SILVUSPROJECT}/${projectname}"
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
	# go to SILVUSPROJECT
	else
		cd "${SILVUSPROJECT}"
	fi
}
