
# Projects root folder (and for Virtualenvwrapper)
export PROJECT_HOME="/data/dev"

# Doc folder
export DOCUMENTATION_HOME="/data/doc"

# Launch simple web server
alias php_server="php -sS localhost:8000"

dev() {
	if [[ -d "$PROJECT_HOME/$1" ]]; then
		cd "$PROJECT_HOME/$1";
		if [[ -d ".git" ]]; then
			git pull origin master
		fi
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

# Golang
# ------------------------------------------------------
if [[ -d "/usr/local/go/bin" ]]; then
    export PATH="$PATH:/usr/local/go/bin"
    if [[ -d "$PROJECT_HOME/go/bin" ]]; then
	    export GOPATH="$PROJECT_HOME/go"
	    export PATH="$PATH:$GOPATH/bin"
    fi
fi
