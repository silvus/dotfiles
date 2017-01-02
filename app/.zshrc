
# Plugins
# -----------------------------------------------------------------------------
# zplug
export ZPLUG_HOME=~/.zsh/zplug
[ -f ${ZPLUG_HOME}/init.zsh ] && source ${ZPLUG_HOME}/init.zsh

# Suggestions
[ -f ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh ] && source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

# Syntax highlighting
if [[ -f ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]; then
	source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
	# Declare the variable
	typeset -A ZSH_HIGHLIGHT_STYLES
	# To have paths colored instead of underlined
	ZSH_HIGHLIGHT_STYLES[path]='fg=green'
fi

# Git prompt
if [[ -f ~/.zsh/zsh-git-prompt/zshrc.sh ]]; then
	source ~/.zsh/zsh-git-prompt/zshrc.sh
	ZSH_THEME_GIT_PROMPT_PREFIX='['
	ZSH_THEME_GIT_PROMPT_SUFFIX=']'
	ZSH_THEME_GIT_PROMPT_CACHE='true'
fi

# Fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

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

# Source files from bash and aliases folder
# ------------------------------------------------------
for file in $SILVUSDOTFILES/shell/aliases/*(.); do
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
[ -f ${SILVUSDOTFILES}/shell/shell_env ] && source ${SILVUSDOTFILES}/shell/shell_env

# Historic compatibility - need to be removed
[ -f ${SILVUSDOTFILES}/shell/bash_env ] && source ${SILVUSDOTFILES}/shell/bash_env
