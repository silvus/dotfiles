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
	# export VIRTUALENVWRAPPER_VIRTUALENV_ARGS='--no-site-packages'
	# Load virtualenvwrapper
	source "$VIRTUALENVWRAPPER_PATH"
	# Lazy virtualenvwrapper
	# source /usr/local/bin/virtualenvwrapper_lazy.sh
fi


# Auto activate virtualenv
# From: "Automatically activate virtualenv" by Kurt Neufeld (http://www.burgundywall.com/tech/automatically-activate-virtualenv/)
# -----------------------------------------------------------------------------
export PREVPWD=`pwd`
export PREVENV_PATH=

handle_virtualenv() {
	if [[ -f "$VIRTUALENVWRAPPER_PATH" ]]; then
		if [ "$PWD" != "$PREVPWD" ]; then
			PREVPWD="$PWD";
			if [ -n "$PREVENV_PATH" ]; then
				if [ "`echo "$PWD" | grep -c $PREVENV_PATH`" = "0"  ]; then
					deactivate
					unalias python 2> /dev/null
					PREVENV_PATH=
				fi
			fi

			# activate virtualenv dynamically
			if [ -e "$PWD/.venv" ] && [ "$PWD" != "$PREVENV_PATH" ]; then
				PREVENV_PATH="$PWD"
				workon `basename $PWD`
				# source "$PWD/.venv"
			fi
		fi
	fi
}

export PROMPT_COMMAND=handle_virtualenv
