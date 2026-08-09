function __fish_prompt_init --description 'One-time setup shared by fish_prompt and fish_right_prompt'
    # Guard flag so this setup only runs once per session, however many times/from
    # whichever prompt function it gets called first
    if set -q __fish_prompt_configured
        return
    end
    set -g __fish_prompt_configured 1

    # Colors
    set -g __fish_color_normal (set_color normal)
    set -g __fish_color_green (set_color green)
    set -g __fish_color_red (set_color red)
    set -g __fish_color_blue (set_color blue)
    set -g __fish_color_cyan (set_color cyan)
    set -g __fish_color_yellow (set_color yellow)
    set -g __fish_color_magenta (set_color magenta)

    # To change the number of characters per path component (defaults to 1)
    if not set -q fish_prompt_pwd_dir_length
        set -g fish_prompt_pwd_dir_length 0
    end

    # Switch user color if root
    switch $USER
        case root toor
            set -g __fish_prompt_color_username $__fish_color_red
        case '*'
            set -g __fish_prompt_color_username $__fish_color_blue
    end

    # Get Hostname
    set -g __fish_prompt_hostname (string split . -- $hostname)[1]

    # Switch hostname color on ssh / VM / container
    if begin
            test -n "$SSH_CLIENT"; or test -n "$SSH_TTY"
        end
        set -g __fish_color_hostname $__fish_color_red
    else if command -sq systemd-detect-virt; and test (systemd-detect-virt) != none
        set -g __fish_color_hostname $__fish_color_magenta
    else
        set -g __fish_color_hostname $__fish_color_blue
    end

    # Git prompt config
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
    set -g __fish_git_prompt_color_untrackedfiles normal
    set -g __fish_git_prompt_color_cleanstate green --bold

    # Once a commandline is submitted, redraw its prompt in a minimal form
    set -g fish_transient_prompt 1
end

function __fish_prompt_status_segment --description 'Colored "<stat> [<exec_time>]" segment shared by both prompts' --argument-names stat duration
    set -l color $__fish_color_green
    if test $stat -gt 0
        set color $__fish_color_red
    end

    # Show execution time next to the status if the last command took more than 2s
    set -l exec_time ""
    if test -n "$duration"; and test "$duration" -gt 2000
        set exec_time " $__fish_color_yellow"(math --scale=1 "$duration / 1000")"s$__fish_color_normal"
    end

    printf '%s%s%s%s' "$color" "$stat" "$__fish_color_normal" "$exec_time"
end

function fish_right_prompt --description 'Write out the right prompt'
    # Save the return status and duration of the previous command
    # (must happen first: $status/$CMD_DURATION reflect the last command run,
    # and would otherwise be clobbered by the commands below)
    set -l stat $status
    set -l duration $CMD_DURATION

    # No right prompt on the transient (already-submitted) prompt line
    if test "$argv[1]" = --final-rendering
        return
    end

    __fish_prompt_init

    set -l __fish_status_segment (__fish_prompt_status_segment $stat $duration)

    # Current time
    set -l __fish_time_status (date +%H:%M:%S)

    set -l __fish_git_status (__fish_git_prompt)

    # Check for NixOS shell
    set -l __fish_nix_shell ""
    if test -n "$IN_NIX_SHELL"
        set -l __fish_color_nix (set_color brmagenta)
        set __fish_nix_shell "[$__fish_color_nix""nix-shell$__fish_color_normal]-"
    end

    printf '%s %s[%s]─[%s%s%s]' "$__fish_git_status" "$__fish_nix_shell" "$__fish_status_segment" "$__fish_color_blue" "$__fish_time_status" "$__fish_color_normal"

end

function fish_prompt --description 'Write out the left prompt'
    # Save the return status and duration of the previous command
    # (must happen first, see fish_right_prompt)
    set -l stat $status
    set -l duration $CMD_DURATION

    __fish_prompt_init

    # Transient (already-submitted) prompt line: [time]─[return_code exec_time] $
    if test "$argv[1]" = --final-rendering
        set -l __fish_status_segment (__fish_prompt_status_segment $stat $duration)
        printf '[%s%s%s]─[%s] $ ' "$__fish_color_blue" (date +%H:%M:%S) "$__fish_color_normal" "$__fish_status_segment"
        return
    end

    # Change $ color if the user hasn't write permissions on the current directory
    set -l __fish_color_permission $__fish_color_normal
    if not test -w "$PWD"
        set __fish_color_permission $__fish_color_yellow
    end

    printf '[%s%s%s@%s%s%s]─[%s%s%s] %s$%s ' "$__fish_prompt_color_username" $USER "$__fish_color_cyan" "$__fish_color_hostname" $__fish_prompt_hostname "$__fish_color_normal" "$__fish_color_green" (prompt_pwd) "$__fish_color_normal" "$__fish_color_permission" "$__fish_color_normal"
end
