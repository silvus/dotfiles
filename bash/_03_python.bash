# Virtualenvwrapper projects root folder
export PROJECT_HOME="/data/dev"

# Launch simple web server
alias server="python3 -m http.server"

# Quick navigate to projects folders
# -----------------------------------------------------------------------------
dev() {
	if [ -d "$PROJECT_HOME/$1" ]; then
		# workon "$1"
		cd "$PROJECT_HOME/$1";
	else
		cd "$PROJECT_HOME";
	fi
}

# Pip
# -----------------------------------------------------------------------------
export PIP_DOWNLOAD_CACHE="$HOME/.pip/cache"
export PIP_REQUIRE_VIRTUALENV=true

# Virtualenvwrapper
# -----------------------------------------------------------------------------
VIRTUALENVWRAPPER_PATH="/usr/local/bin/virtualenvwrapper.sh"
if [[ -f "$VIRTUALENVWRAPPER_PATH" ]]; then
	# Python version to use
	export VIRTUALENVWRAPPER_PYTHON="/usr/bin/python3"
	# Virtualenvwrapper virtualenvs folder
	export WORKON_HOME="$PROJECT_HOME/.virtualenvs"
	# Virtualenvwrapper args
	# export VIRTUALENVWRAPPER_VIRTUALENV_ARGS='--no-site-packages'
	# Load virtualenvwrapper
	source "$VIRTUALENVWRAPPER_PATH"
fi
