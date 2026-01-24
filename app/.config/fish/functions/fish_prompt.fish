function fish_right_prompt --description 'Write out the right prompt'
    # Save the return status of the previous command
    set stat $status
    # Set the color for the status depending on the value
    set __fish_color_status (set_color green)
    if test $stat -gt 0
        set __fish_color_status (set_color red)
    end
    if not set -q __fish_color_normal
        set -g __fish_color_normal (set_color normal)
    end

    # Current time
    set __fish_time_status (date +%H:%M:%S)

    # Git prompt
    set -g __fish_git_prompt_show_informative_status 1
    set -g __fish_git_prompt_showdirtystate 1
    set -g __fish_git_prompt_showstashstate 1
    set -g __fish_git_prompt_showuntrackedfiles 1
    set -g __fish_git_prompt_showupstream informative

    set -g __fish_git_prompt_color_branch yellow
    set -g __fish_git_prompt_char_upstream_ahead "↑"
    set -g __fish_git_prompt_char_upstream_behind "↓"
    set -g __fish_git_prompt_char_upstream_prefix ""

    set -g __fish_git_prompt_char_stagedstate "●"
    set -g __fish_git_prompt_char_dirtystate "✚"
    set -g __fish_git_prompt_char_untrackedfiles "…"
    set -g __fish_git_prompt_char_conflictedstate "✖"
    set -g __fish_git_prompt_char_cleanstate "✔"

    set -g __fish_git_prompt_color_dirtystate blue
    set -g __fish_git_prompt_color_stagedstate yellow
    set -g __fish_git_prompt_color_invalidstate red
    set -g __fish_git_prompt_color_untrackedfiles $fish_color_normal
    set -g __fish_git_prompt_color_cleanstate green --bold

    # Check if git status is empty
    set __fish_git_status (__fish_git_prompt)
    if test -z $__fish_git_status
        set __fish_git_status ""
    end

    # Check for NixOS shell
    set __fish_nix_shell ""
    if test -n "$IN_NIX_SHELL"
        set -g __fish_color_nix (set_color brmagenta)
        set __fish_nix_shell "[$__fish_color_nix""nix-shell$__fish_color_normal]-"
    end

    printf '%s %s[%s%s%s]─[%s%s%s]' "$__fish_git_status" "$__fish_nix_shell" "$__fish_color_status" "$stat" "$__fish_color_normal" "$__fish_color_blue" "$__fish_time_status" "$__fish_color_normal"

end

function fish_prompt --description 'Write out the left prompt'
    # To change the number of characters per path component (defaults to 1)
    if not set -q fish_prompt_pwd_dir_length
        set -g fish_prompt_pwd_dir_length 0
    end

    # Just calculate these once, to save a few cycles when displaying the prompt

    # Colors
    if not set -q __fish_color_normal
        set -g __fish_color_normal (set_color normal)
    end
    if not set -q __fish_color_green
        set -g __fish_color_green (set_color green)
    end
    if not set -q __fish_color_red
        set -g __fish_color_red (set_color red)
    end
    if not set -q __fish_color_blue
        set -g __fish_color_blue (set_color blue)
    end
    if not set -q __fish_color_cyan
        set -g __fish_color_cyan (set_color cyan)
    end
    if not set -q __fish_color_yellow
        set -g __fish_color_yellow (set_color yellow)
    end

    # Switch user color if root
    if not set -q __fish_prompt_color_username
        switch $USER
            case root toor
                set -g __fish_prompt_color_username $__fish_color_red
            case '*'
                set -g __fish_prompt_color_username $__fish_color_blue
        end
    end

    # Get Hostname
    if not set -q __fish_prompt_hostname
        set -g __fish_prompt_hostname (hostname|cut -d . -f 1)
    end

    # Switch hostname color on ssh
    if not set -q __fish_color_hostname
        if begin
                test -n "$SSH_CLIENT"; or test -n "$SSH_TTY"
            end
            set -g __fish_color_hostname $__fish_color_red
        else
            set -g __fish_color_hostname $__fish_color_blue
        end
    end

    # Change $ color if the user hasn't write permissions on the current directory
    if test -w "$PWD"
        set -g __fish_color_permission $__fish_color_normal
    else
        set -g __fish_color_permission $__fish_color_yellow
    end

    # printf '[%s] %s%s%s@%s%s %s%s %s[%s]%s \f\r$ ' (date "+%H:%M:%S") "$__fish_color_blue" $USER "$__fish_color_yellow" "$__fish_color_blue" $__fish_prompt_hostname "$__fish_prompt_cwd" "$PWD" "$__fish_color_status" "$stat" "$__fish_color_normal"
    printf '[%s%s%s@%s%s%s]─[%s%s%s] %s$%s ' "$__fish_prompt_color_username" $USER "$__fish_color_cyan" "$__fish_color_hostname" $__fish_prompt_hostname "$__fish_color_normal" "$__fish_color_green" (prompt_pwd) "$__fish_color_normal" "$__fish_color_permission" "$__fish_color_normal"
end
