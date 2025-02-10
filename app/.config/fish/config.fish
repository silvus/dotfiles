# Init
# -----------------------------------------------------------------------------
# Source .profile with the help of Foreign Environment Plugin https://github.com/oh-my-fish/plugin-foreign-env
fenv source ~/.profile

# Stop changing clipboard content on kill
# https://github.com/fish-shell/fish-shell/issues/772
set --global -x FISH_CLIPBOARD_CMD "cat"


# Fish style # https://fishshell.com/docs/current/index.html#variables-color
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
alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../../'
alias ....='cd ../../../'
alias .....='cd ../../../../'
alias ......='cd ../../../../../'

alias l='env LC_COLLATE=C ls -lhaFN --color=auto --group-directories-first'

function mkcd --description 'Create a folder and go into it'
    mkdir -p "$argv"
    cd "$argv"
end

alias diskusage='ncdu'
alias calculator='bc -l'
alias copy='xclip -selection clipboard'
alias copytoclipboard='xclip -selection clipboard'
alias resolution='xrandr -q | grep "*" | cut -d " " -f 4'
alias keymapazertyqwerty="setxkbmap -model pc105 -layout fr,gb -variant oss,intl -option \"grp:shift_caps_toggle,grp_led:scroll,nbsp:level4,lv3:ralt_switch,compose:menu,eurosign:e\""
alias bat='batcat'
alias fd='fdfind'
alias lz='lazygit'

alias tree1="tree --dirsfirst -ChFLQ 1"
alias tree2="tree --dirsfirst -ChFLQ 2"
alias tree3="tree --dirsfirst -ChFLQ 3"
alias tree4="tree --dirsfirst -ChFLQ 4"
alias tree5="tree --dirsfirst -ChFLQ 5"
alias tree6="tree --dirsfirst -ChFLQ 6"


# Nvim profiles
# -----------------------------------------------------------------------------
alias v='nvim'

alias nvim-custom='NVIM_APPNAME=nvim-custom nvim'
alias nvim-kickstart='NVIM_APPNAME=nvim-kickstart nvim'
alias nvim-lazyvim='NVIM_APPNAME=nvim-lazyvim nvim'
alias nvim-nvchad='NVIM_APPNAME=nvim-nvchad nvim'
alias nvim-astrovim='NVIM_APPNAME=nvim-astrovim nvim'
alias nvim-lunarvim='NVIM_APPNAME=nvim-lunarvim nvim'


# Xset
# -----------------------------------------------------------------------------
# Set keyboard repeat delay and rate
if type -q xset
    # If X is running
    if xset q &>/dev/null
        # Default: xset r rate 660 25
        xset r rate 200 30

        # Disable beeps
        xset b off
    end
end


# Bindings
# -----------------------------------------------------------------------------
bind \ce fzf-editor-open

# Fzf trigger file with CTRL+P
bind \cp fzf-file-widget

# Fzf trigger history with CTRL+H
bind \ch fzf-history-widget


# Environment specific configuration
# -----------------------------------------------------------------------------
if test -f "$SILVUSDOTFILES_CUSTOM/shellfish"
    source "$SILVUSDOTFILES_CUSTOM/shellfish"
end
