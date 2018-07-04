# TTY theme (https://www.reddit.com/r/unixporn/comments/55kjdb/tmux_tty_with_custom_theme/)

# /*COLOR         DESC.         H    S   l */
# "#1A1813",  /*  0: black     45,  25, 10 */
# "#991f1f",  /*  1: red        0,  80, 60 */
# "#5C991F",  /*  2: green     90,  80, 60 */
# "#997B1F",  /*  3: yellow    45,  80, 60 */
# "#1F3E99",  /*  4: blue     225,  80, 60 */
# "#991F70",  /*  5: magenta  320,  80, 60 */
# "#1F9999",  /*  6: cyan     180,  80, 60 */
# "#CCBC95",  /*  7: white     45,  25, 80 */
#
# /*Brighter colors*/
# "#333026",  /*  0: black     45,  25,  20 */
# "#E62E2E",  /*  1: red        0,  100, 80 */
# "#8AE62E",  /*  2: green     90,  100, 80 */
# "#E6B82E",  /*  3: yellow    45,  100, 80 */
# "#2E5CE6",  /*  4: blue     225,  100, 80 */
# "#E62EA9",  /*  5: magenta  320,  100, 80 */
# "#2EE6E6",  /*  6: cyan     180,  100, 80 */
# "#E6D7AB",  /*  7: white     45,  25,  90 */

if [ "$TERM" = "linux" ]; then
    # printf '\033]P01a1813'; # black
    printf '\033]P000060f'; # black
    printf '\033]P1991f1f'; # red
    printf '\033]P25c991f'; # green
    printf '\033]P3997b1f'; # yellow
    printf '\033]P41f3e99'; # blue
    printf '\033]P5991f70'; # magenta
    printf '\033]P61f9999'; # cyan
    printf '\033]P7ccbc95'; # white
    printf '\033]P8333026'; # brighter black
    printf '\033]P9E62E2E'; # brighter red
    printf '\033]PA8AE62E'; # brighter green
    printf '\033]PBE6B82E'; # brighter yellow
    printf '\033]PC2E5CE6'; # brighter blue
    printf '\033]PDE62EA9'; # brighter magenta
    printf '\033]PE2EE6E6'; # brighter cyan
    printf '\033]PFE6D7AB'; # brighter white
    clear; # for background artifacting
fi;
