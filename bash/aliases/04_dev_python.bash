
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
