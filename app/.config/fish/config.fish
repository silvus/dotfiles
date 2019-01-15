# Init
# -----------------------------------------------------------------------------
# NOPE: Need a most recent fish shell (2.6.0) to use bass
# For now I will let bash init fish, so .profile are sourced and other env variables are set
# Aliases are not herited
#
# if status --is-interactive
# 	# Disable flow control (bind Ctrl+s in vim)
# 	stty -ixon
# 	# Stop backward-kill-word on directory delimiter (bind Ctrl+w in bash / zsh)
# 	stty werase undef
# end
#
# Source env variables
# bass source ~/.dotfiles/shell/aliases/10_env.sh

# Export a global env variable to tel bash to not start fish again (if we try to launch a bash shell inside a fish shell)
# ($FISH_VERSION doesn't work outside of fish ?)
set --global -x FISH_IS_STARTED 1


# Fish style
# https://fishshell.com/docs/current/index.html#variables-color
# -----------------------------------------------------------------------------
# fish_color_normal, the default color
# fish_color_command, the color for commands
# fish_color_quote, the color for quoted blocks of text
# fish_color_redirection, the color for IO redirections
# fish_color_end, the color for process separators like ';' and '&'
# fish_color_error, the color used to highlight potential errors
# fish_color_param, the color for regular command parameters
# fish_color_comment, the color used for code comments
# fish_color_match, the color used to highlight matching parenthesis
# fish_color_selection, the color used when selecting text (in vi visual mode)
# fish_color_search_match, used to highlight history search matches and the selected pager item (must be a background)
set --global fish_color_search_match --background='green'
# fish_color_operator, the color for parameter expansion operators like '*' and '~'
# fish_color_escape, the color used to highlight character escapes like '\n' and '\x70'
# fish_color_cwd, the color used for the current working directory in the default prompt
# fish_color_autosuggestion, the color used for autosuggestions
# fish_color_user, the color used to print the current username in some of fish default prompts
# fish_color_host, the color used to print the current host system in some of fish default prompts
# fish_color_cancel, the color for the '^C' indicator on a canceled command

# Additionally, the following variables are available to change the highlighting in the completion pager:
# fish_pager_color_prefix, the color of the prefix string, i.e. the string that is to be completed
# fish_pager_color_completion, the color of the completion itself
# fish_pager_color_description, the color of the completion description
# fish_pager_color_progress, the color of the progress bar at the bottom left corner
# fish_pager_color_secondary, the background color of the every second completion


# Aliases
# -----------------------------------------------------------------------------
# Source bash aliases
source ~/.dotfiles/shell/aliases/20_main.sh

# Differents alias
alias l='env LC_COLLATE=C ls -lhaF --color=auto --group-directories-first'

function mkcd --description 'Create a folder and go into it'
	mkdir -p "$argv"
	cd "$argv"
end

