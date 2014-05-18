PROJECTS_FOLDER="/data/dev"

# Launch simple web server
alias server="python3 -m http.server"

# Quick navigate to projects folders
# -----------------------------------------------------------------------------
dev() {
	if [ -d "$PROJECTS_FOLDER/$1" ]; then
		# workon "$1"
		cd "$PROJECTS_FOLDER/$1";
	else
		cd "$PROJECTS_FOLDER";
	fi
}

# Pip
# -----------------------------------------------------------------------------
export PIP_DOWNLOAD_CACHE=$HOME/.pip/cache
export PIP_REQUIRE_VIRTUALENV=true

# Virtualenvwrapper
# -----------------------------------------------------------------------------
VIRTUALENVWRAPPER_PATH="/usr/local/bin/virtualenvwrapper.sh"
if [[ -f "$VIRTUALENVWRAPPER_PATH" ]]; then
	# Python version to use
	export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
	# Virtualenvwrapper virtualenvs folder
	export WORKON_HOME=$PROJECTS_FOLDER/.virtualenvs
	# Virtualenvwrapper projects root folder
	export PROJECT_HOME=$PROJECTS_FOLDER
	# Virtualenvwrapper args
	# export VIRTUALENVWRAPPER_VIRTUALENV_ARGS='--no-site-packages'
	# Load virtualenvwrapper
	source "$VIRTUALENVWRAPPER_PATH"
fi
