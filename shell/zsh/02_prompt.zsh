# Plugin git
if [[ -f ~/.zsh/zsh-git-prompt/zshrc.sh ]]; then
	source ~/.zsh/zsh-git-prompt/zshrc.sh
	ZSH_THEME_GIT_PROMPT_PREFIX='['
	ZSH_THEME_GIT_PROMPT_SUFFIX=']'
	ZSH_THEME_GIT_PROMPT_CACHE='true'
fi

setopt promptsubst
autoload -U colors && colors

# Prompt commands
# -----------------------------------------------------------------------------

# Like PROMPT_COMMAND in bash
precmd() {
	# if the user is root
	if [[ "$EUID" -eq 0 ]]; then
		local symbol='#'
	else
		local symbol='$'
	fi

	# Change color if the user hasn't write permissions on the current directory
	if [[ -w "${PWD}" ]]; then
		_PROMPT_SYMBOL="${symbol}"
	else
		_PROMPT_SYMBOL="%{$fg[yellow]%}${symbol}%{$reset_color%}"
	fi
}

# Change color if the user hasn't write permissions on the current directory
_prompt_symbol() {
	echo "${_PROMPT_SYMBOL}"
}

# Check if python virtualenv is active
_prompt_python_venv() {
	if [ ! -z "$VIRTUAL_ENV" ]; then
		echo "[%{$fg[magenta]%}$(basename "$(dirname "${VIRTUAL_ENV}")")%{$reset_color%}]"
	fi
}

function() {
	# Prompt
	# --------------------------------------------------------------------------------------
	local current_dir='[%{$fg[blue]%}%~%{$reset_color%}]'
	local current_time='[%{$fg[green]%}%D{%T}%{$reset_color%}]'
	local symbol='$(_prompt_symbol)'

	# Change host color if connected by ssh
	if [[ -n "$SSH_CLIENT" ]] || [[ -n "$SSH_TTY" ]]; then
		local host='%{$fg[yellow]%}@%{$fg[red]%}%m%{$reset_color%}]'
	else
		local host='%{$fg[yellow]%}@%{$fg[green]%}%m%{$reset_color%}]'
	fi

	# Change color if the user is root
	if [[ "$EUID" -eq 0 ]]; then
		local user='[%{$fg[magenta]%}%n%{$reset_color%}'
	else
		local user='[%{$fg[green]%}%n%{$reset_color%}'
	fi

	PROMPT="${current_time}─${user}${host}─${current_dir} ${symbol} "

	# Right Prompt
	# --------------------------------------------------------------------------------------
	local git_branch='$(git_super_status)%{$reset_color%}'
	local return_code='%(?..[%{$fg[red]%}%? ↵%{$reset_color%}])'
	local python_venv='$(_prompt_python_venv)'

	RPROMPT="${return_code}${python_venv}${git_branch}"
}
