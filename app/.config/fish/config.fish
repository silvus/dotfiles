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

alias e='$EDITOR'
alias b='$BROWSER'
alias diskusage='ncdu'
alias calculator='bc -l'
alias copy='xclip -selection clipboard'
alias copytoclipboard='xclip -selection clipboard'
alias resolution='xrandr -q | grep "*" | cut -d " " -f 4'
alias keymapazertyqwerty="setxkbmap -model pc105 -layout fr,gb -variant oss,intl -option \"grp:shift_caps_toggle,grp_led:scroll,nbsp:level4,lv3:ralt_switch,compose:menu,eurosign:e\""
alias fd='fdfind'
alias lz='lazygit'

if type -q batcat
    alias bat='batcat'
end


alias tree1="tree --dirsfirst -ChFLQ 1"
alias tree2="tree --dirsfirst -ChFLQ 2"
alias tree3="tree --dirsfirst -ChFLQ 3"
alias tree4="tree --dirsfirst -ChFLQ 4"
alias tree5="tree --dirsfirst -ChFLQ 5"
alias tree6="tree --dirsfirst -ChFLQ 6"


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
    if read -z cwd < "$tmp"; and [ -n "$cwd" ]; and [ "$cwd" != "$PWD" ]
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


# TTY theme (https://www.reddit.com/r/unixporn/comments/55kjdb/tmux_tty_with_custom_theme/)
# -----------------------------------------------------------------------------
# /*COLOR         DESC.         H    S   l */
# "#1A1813",  /*  0: black     45,  25, 10 */
# "#991f1f",  /*  1: red        0,  80, 60 */
# "#5C991F",  /*  2: green     90,  80, 60 */
# "#997B1F",  /*  3: yellow    45,  80, 60 */
# "#2e53bf",  /*  4: blue     225,  80, 60 */
# "#991F70",  /*  5: magenta  320,  80, 60 */
# "#1F9999",  /*  6: cyan     180,  80, 60 */
# "#e6e6e6",  /*  7: white     45,  25, 80 */
#
# /*Brighter colors*/
# "#333026",  /*  0: black     45,  25,  20 */
# "#E62E2E",  /*  1: red        0,  100, 80 */
# "#8AE62E",  /*  2: green     90,  100, 80 */
# "#E6B82E",  /*  3: yellow    45,  100, 80 */
# "#285cf7",  /*  4: blue     225,  100, 80 */
# "#E62EA9",  /*  5: magenta  320,  100, 80 */
# "#2EE6E6",  /*  6: cyan     180,  100, 80 */
# "#FFFFFF",  /*  7: white     45,  25,  90 */

if test "$TERM" = "linux"
    printf '\033]P000060f'  # black
    printf '\033]P1991f1f'  # red
    printf '\033]P25c991f'  # green
    printf '\033]P3997b1f'  # yellow
    printf '\033]P42e53bf'  # blue
    printf '\033]P5991f70'  # magenta
    printf '\033]P61f9999'  # cyan
    printf '\033]P7e6e6e6'  # white
    printf '\033]P8333026'  # brighter black
    printf '\033]P9E62E2E'  # brighter red
    printf '\033]PA8AE62E'  # brighter green
    printf '\033]PBE6B82E'  # brighter yellow
    printf '\033]PC285cf7'  # brighter blue
    printf '\033]PDE62EA9'  # brighter magenta
    printf '\033]PE2EE6E6'  # brighter cyan
    printf '\033]PFFFFFFF'  # brighter white
    clear  # for background artifacting
end


# Environment specific configuration
# -----------------------------------------------------------------------------
if test -f "$SILVUSDOTFILES_CUSTOM/shellfish"
    source "$SILVUSDOTFILES_CUSTOM/shellfish"
end

