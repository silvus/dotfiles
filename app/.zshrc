
if [[ ! -d ~/.zsh ]];then
	mkdir ~/.zsh
fi

# Plugins
# -----------------------------------------------------------------------------
# zplug
export ZPLUG_HOME=~/.zsh/zplug
if [[ ! -d ${ZPLUG_HOME} ]];then
	printf "Install zplug? [y/N]: "
	if read -q; then
		echo; git clone https://github.com/zplug/zplug.git ${ZPLUG_HOME}
	fi
fi

if [[ -f ${ZPLUG_HOME}/init.zsh ]]; then
	source ${ZPLUG_HOME}/init.zsh

	zplug "zsh-users/zsh-autosuggestions"
	zplug "zsh-users/zsh-completions"
	zplug "olivierverdier/zsh-git-prompt", use:"zshrc.sh"
	zplug "junegunn/fzf", use:"shell/*.zsh"
	zplug "junegunn/fzf", use:"bin/fzf-tmux", as:command
	# Grab binaries from GitHub Releases and rename with the "rename-to:" tag
	zplug "junegunn/fzf-bin", \
		from:gh-r, \
		as:command, \
		rename-to:fzf

	# zsh-syntax-highlighting must be loaded after executing compinit command and sourcing other plugins
	# (If the defer tag is given 2 or above, run after compinit command)
	zplug "zsh-users/zsh-syntax-highlighting", defer:2

	# Install plugins if there are plugins that have not been installed
	if ! zplug check --verbose; then
		printf "Install? [y/N]: "
		if read -q; then
			echo; zplug install
		fi
	fi

	# Then, source plugins and add commands to $PATH
	# zplug load --verbose
	zplug load
fi

# zsh-syntax-highlighting
# To have paths colored instead of underlined
typeset -A ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_STYLES[path]='fg=green'

# zsh-git-prompt
ZSH_THEME_GIT_PROMPT_PREFIX='['
ZSH_THEME_GIT_PROMPT_SUFFIX=']'
ZSH_THEME_GIT_PROMPT_CACHE='true'

# Fzf
# [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Command not found like in bash
[[ -a "/etc/zsh_command_not_found" ]] && . /etc/zsh_command_not_found

# History
# -----------------------------------------------------------------------------
setopt histignorealldups histignorespace sharehistory
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh/history

# Completion
# -----------------------------------------------------------------------------
autoload -Uz compinit
compinit

setopt MENU_COMPLETE
setopt COMPLETE_ALIASES
setopt COMPLETE_IN_WORD    # Complete from both ends of a word.
setopt ALWAYS_TO_END       # Move cursor to the end of a completed word.
setopt PATH_DIRS           # Perform path search even on command names with slashes.
setopt EXTENDED_GLOB
# setopt AUTO_MENU           # Show completion menu on a succesive tab press.
# setopt AUTO_LIST           # Automatically list choices on ambiguous completion.
setopt AUTO_PARAM_SLASH    # If completed parameter is a directory, add a trailing slash.
# unsetopt MENU_COMPLETE     # Do not autoselect the first completion entry.
unsetopt FLOW_CONTROL      # Disable start/stop characters in shell editor.
# unsetopt nomatch # prevent ZSH to print an error when no match can be found in a glob
# first autocompletion is not filled out automatically
setopt noautomenu
setopt nomenucomplete
# Don't complete unavailable commands.
# zstyle ':completion:*:functions' ignored-patterns '(_*|pre(cmd|exec))'

zstyle ':completion:*' auto-description 'specify: %d'
# zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' completer _expand _complete
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
# eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
# zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
# zstyle ':completion:*' menu select=long
# zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

# Ignore multiple entries.
zstyle ':completion:*:(rm|kill|diff):*' ignore-line yes
zstyle ':completion:*:rm:*' file-patterns '*:all-files'

# Kill
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# Man
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true

# Media
zstyle ':completion:*:*:mocp:*' file-patterns '*.(wav|WAV|mp3|MP3|ogg|OGG|flac):ogg\ files *(-/):directories'

# Use caching to make completion for cammands such as dpkg and apt usable.
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path "$HOME/.zsh/cache"

# Group matches and describe.
zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:corrections' format ' %F{green}-- %d (errors: %e) --%f'
zstyle ':completion:*:descriptions' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*:messages' format ' %F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' format ' %F{yellow}-- %d --%f'

# Stop backward-kill-word on directory delimiter (Ctrl + w)
# -----------------------------------------------------------------------------
autoload -U select-word-style
select-word-style bash

# -----------------------------------------------------------------------------
export SILVUSDOTFILES=~/.dotfiles
export SILVUSDOTFILES_CUSTOM=~/.dotfiles_custom

# Source files from bash and aliases folder
# ------------------------------------------------------
for file in ${SILVUSDOTFILES}/shell/aliases/*(.); do
	if [[ -f "$file" ]]; then
		source "$file"
	fi
done

for file in ${SILVUSDOTFILES}/shell/zsh/*(.); do
	if [[ -f "$file" ]]; then
		source "$file"
	fi
done


# Environment specific configuration
# -----------------------------------------------------------------------------
[ -f ${SILVUSDOTFILES_CUSTOM}/shell ] && source ${SILVUSDOTFILES_CUSTOM}/shell
