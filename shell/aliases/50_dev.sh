# Python
# ------------------------------------------------------
export PYTHONSTARTUP="$HOME/.pythonrc"
# export PYTHONPATH="./.venv:$PYTHONPATH"

# Pip
export PIP_DOWNLOAD_CACHE="$HOME/.pip/cache"
pipvendor() {
   pip3 "$@" -t ".venv"
}

# Pew / Virtualenvwrapper
export WORKON_HOME="$SILVUSPROJECT/.venvs"

# Virtualenv
export VIRTUAL_ENV_DISABLE_PROMPT=true # Don't touch my PS1
alias activate="source .venv/bin/activate"
alias pythonvenv=".venv/bin/python3"
alias pythonvenvcreate="python3 -m venv .venv; source .venv/bin/activate"
alias pythonvenvpip=".venv/bin/pip3"

# Pipsi
export PIPSI_BIN_DIR="$HOME/bin"

# Pyenv (Python version management)
export PYENV_ROOT="$HOME/.config/pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
	eval "$(pyenv init -)"
fi

# Rust
# ------------------------------------------------------
export RUSTUP_HOME="$HOME/.config/rustup"
export CARGO_HOME="$HOME/.config/cargo"
export PATH="${CARGO_HOME}/bin:$PATH"


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

# Small utilities function
# Must be here due to "cd" subshell usage
# ------------------------------------------------------------
mkcd() { mkdir -p "$@" && cd "$_"; }

tmpfile() { $EDITOR $(mktemp); }
tmpdir() { cd $(mktemp -d); }
