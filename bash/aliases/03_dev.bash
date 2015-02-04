
# Dotfiles
alias dot="cd $DOTFILES_PATH"

# Projects root folder (and for Virtualenvwrapper)
export PROJECT_HOME="/data/dev"

# Doc folder
export DOCUMENTATION_HOME="/data/doc"

# Launch simple web server
alias server_php="php -sS localhost:8000"
alias server_python="python3 -m http.server 8000"

dev() {
	if [[ -d "$PROJECT_HOME/$1" ]]; then
		cd "$PROJECT_HOME/$1";
		if [[ -d ".git" ]]; then
			git pull origin master
		fi
	else
		cd "$PROJECT_HOME"
	fi
}
