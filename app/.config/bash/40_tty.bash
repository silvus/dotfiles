# TTY colour scheme (Tango like) (https://www.reddit.com/r/unixporn/comments/55kjdb/tmux_tty_with_custom_theme/)
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

if [ "$TERM" = "linux" ]; then
    printf '\033]P01a1a1a'  # black
    printf '\033]P1cc0000'  # red
    printf '\033]P24e9a06'  # green
    printf '\033]P3c4a000'  # yellow
    printf '\033]P43465a4'  # blue
    printf '\033]P575507b'  # magenta
    printf '\033]P606989a'  # cyan
    printf '\033]P7d3d7cf'  # white
    printf '\033]P8555753'  # bright black
    printf '\033]P9ef2929'  # bright red
    printf '\033]PA8ae234'  # bright green
    printf '\033]PBfce94f'  # bright yellow
    printf '\033]PC729fcf'  # bright blue
    printf '\033]PDad7fa8'  # bright magenta
    printf '\033]PE34e2e2'  # bright cyan
    printf '\033]PFeeeeec'  # bright white
    clear
fi;

