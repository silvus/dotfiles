# Enforce correct locales from the beginning:
# LC_ALL is unset since it overwrites everything
# LANG is used, except for:
# LC_MESSAGES : never translates program output
# LC_TIME : leads to dd-mm-yyyy hh:mm date/time output
unset LC_ALL
export LANG=en_US.UTF-8
export LC_MESSAGES=C
export LC_TIME=fr_FR.UTF-8
