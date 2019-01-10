
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


# Source bash aliases
source ~/.dotfiles/shell/aliases/20_main.sh

# Differents alias
alias l='env LC_COLLATE=C ls -lhaF --color=auto --group-directories-first'

# TODO
# mkcd() { mkdir -p "$@" && cd "$_"; }
# tmpfile() { $EDITOR $(mktemp); }
# tmpdir() { cd $(mktemp -d); }
