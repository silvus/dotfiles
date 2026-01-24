# Init
# -----------------------------------------------------------------------------
# Source .profile with the help of Foreign Environment Plugin https://github.com/oh-my-fish/plugin-foreign-env
fenv source ~/.profile

# Stop changing clipboard content on kill
# https://github.com/fish-shell/fish-shell/issues/772
set --global -x FISH_CLIPBOARD_CMD cat

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

function mkcd --description 'Create a folder and go into it'
    mkdir -p "$argv"
    cd "$argv"
end

alias b='$BROWSER'
alias keymapazertyqwerty="setxkbmap -model pc105 -layout fr,gb -variant oss,intl -option \"grp:shift_caps_toggle,grp_led:scroll,nbsp:level4,lv3:ralt_switch,compose:menu,eurosign:e\""
alias fd='fdfind'

if type -q batcat
    alias bat='batcat'
end

alias tree1="tree --dirsfirst -ChFLQ 1"
alias tree2="tree --dirsfirst -ChFLQ 2"
alias tree3="tree --dirsfirst -ChFLQ 3"
alias tree4="tree --dirsfirst -ChFLQ 4"
alias tree5="tree --dirsfirst -ChFLQ 5"
alias tree6="tree --dirsfirst -ChFLQ 6"

# Remove the alias and ensure my custom ls script is used
functions -e l

# Nvim profiles
# -----------------------------------------------------------------------------
alias nvim-lazyvim='NVIM_APPNAME=lazyvim nvim'
alias lnvim='NVIM_APPNAME=lazyvim nvim'

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

# FZF from Nix https://nixos.wiki/wiki/Fzf
# -----------------------------------------------------------------------------
function fish_user_key_bindings
    if command -s fzf-share >/dev/null
        source (fzf-share)/key-bindings.fish
    end

    fzf_key_bindings
end

# yazi Shell wrapper to cd
# -----------------------------------------------------------------------------
function y
    set tmp (mktemp -t "yazi-cwd.XXXXXX")
    yazi $argv --cwd-file="$tmp"
    if read -z cwd <"$tmp"; and [ -n "$cwd" ]; and [ "$cwd" != "$PWD" ]
        builtin cd -- "$cwd"
    end
    rm -f -- "$tmp"
end

# Bindings
# -----------------------------------------------------------------------------
# Fzf search current dir in EDITOR with CTRL+E
bind \ce fzf-editor-open

# Fzf trigger file with CTRL+P
bind \cp fzf-file-widget

# Fzf trigger history with CTRL+H
bind \ch fzf-history-widget

# Fzf search doc in EDITOR with CTRL+D
# bind \co 'fzf-editor-open /data/doc'

# Open TODO.md with CTRL+t
# bind \ct 'tmux_sessionizer -p "/data/doc/todo.org"'

# TTY colour scheme (Tango like)
# -----------------------------------------------------------------------------
# Standard colors
# "#2e1a1a1a",  /*  0: black     */
# "#cc0000",  /*  1: red       */
# "#4e9a06",  /*  2: green     */
# "#c4a000",  /*  3: yellow    */
# "#3465a4",  /*  4: blue      */
# "#75507b",  /*  5: magenta   */
# "#06989a",  /*  6: cyan      */
# "#d3d7cf",  /*  7: white     */

# Brighter colors
# "#555753",  /*  8: bright black   */
# "#ef2929",  /*  9: bright red     */
# "#8ae234",  /* 10: bright green   */
# "#fce94f",  /* 11: bright yellow  */
# "#729fcf",  /* 12: bright blue    */
# "#ad7fa8",  /* 13: bright magenta */
# "#34e2e2",  /* 14: bright cyan    */
# "#eeeeec",  /* 15: bright white   */

if test "$TERM" = linux
    printf '\033]P01a1a1a' # black
    printf '\033]P1cc0000' # red
    printf '\033]P24e9a06' # green
    printf '\033]P3c4a000' # yellow
    printf '\033]P43465a4' # blue
    printf '\033]P575507b' # magenta
    printf '\033]P606989a' # cyan
    printf '\033]P7d3d7cf' # white
    printf '\033]P8555753' # bright black
    printf '\033]P9ef2929' # bright red
    printf '\033]PA8ae234' # bright green
    printf '\033]PBfce94f' # bright yellow
    printf '\033]PC729fcf' # bright blue
    printf '\033]PDad7fa8' # bright magenta
    printf '\033]PE34e2e2' # bright cyan
    printf '\033]PFeeeeec' # bright white
    clear
end

# Environment specific configuration
# -----------------------------------------------------------------------------
if test -f "$SILVUSDOTFILES_CUSTOM/shellfish"
    source "$SILVUSDOTFILES_CUSTOM/shellfish"
end
