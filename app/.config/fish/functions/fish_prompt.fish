function prompt_pwd_full
    # prompt_pwd is broken (https://stackoverflow.com/questions/33714385/how-do-you-change-fish-pwd-length-in-prompt-using-fish-prompt-pwd-dir-length)
    set -q fish_prompt_pwd_dir_length; or set -l fish_prompt_pwd_dir_length 1

    if [ $fish_prompt_pwd_dir_length -eq 0 ]
        set -l fish_prompt_pwd_dir_length 99999
    end

    set -l realhome ~
    echo $PWD | sed -e "s|^$realhome|~|" -e 's-\([^/.]{'"$fish_prompt_pwd_dir_length"'}\)[^/]*/-\1/-g'
end

function fish_right_prompt --description 'Write out the left prompt'
    # Save the return status of the previous command
    set stat $status
     # Set the color for the status depending on the value
    set __fish_color_status (set_color green)
    if test $stat -gt 0
        set __fish_color_status (set_color red)
    end
    if not set -q __fish_prompt_normal
        set -g __fish_prompt_normal (set_color normal)
    end

	set -g __fish_git_prompt_show_informative_status 1
	set -g __fish_git_prompt_showdirtystate 1
	set -g __fish_git_prompt_showstashstate 1
	set -g __fish_git_prompt_showuntrackedfiles 1
	set -g __fish_git_prompt_showupstream "informative"

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

	printf '%s [%s%s%s]' "$__fish_git_status" "$__fish_color_status" "$stat" "$__fish_prompt_normal"
end

function fish_prompt --description 'Write out the right prompt'
    # Just calculate these once, to save a few cycles when displaying the prompt
    if not set -q __fish_prompt_hostname
        set -g __fish_prompt_hostname (hostname|cut -d . -f 1)
    end

    if not set -q __fish_prompt_normal
        set -g __fish_prompt_normal (set_color normal)
    end

    if not set -q __fish_color_green
        set -g __fish_color_green (set_color green)
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

	switch $USER

		case root toor

			if not set -q __fish_prompt_cwd
		        if set -q fish_color_cwd_root
		            set -g __fish_prompt_cwd (set_color $fish_color_cwd_root)
		        else
		            set -g __fish_prompt_cwd (set_color $fish_color_cwd)
		        end
		    end

			printf '%s@%s %s%s%s# ' $USER $__fish_prompt_hostname "$__fish_prompt_cwd" (prompt_pwd) "$__fish_prompt_normal"

		case '*'

			if not set -q __fish_prompt_cwd
	            set -g __fish_prompt_cwd (set_color $fish_color_cwd)
	        end

			# printf '[%s] %s%s%s@%s%s %s%s %s[%s]%s \f\r$ ' (date "+%H:%M:%S") "$__fish_color_blue" $USER "$__fish_color_yellow" "$__fish_color_blue" $__fish_prompt_hostname "$__fish_prompt_cwd" "$PWD" "$__fish_color_status" "$stat" "$__fish_prompt_normal"
			printf '[%s%s%s@%s%s%s]─[%s%s%s] $ ' "$__fish_color_blue" $USER "$__fish_color_cyan" "$__fish_color_blue" $__fish_prompt_hostname "$__fish_prompt_normal" "$__fish_color_green" (prompt_pwd_full) "$__fish_prompt_normal"

	end
end
