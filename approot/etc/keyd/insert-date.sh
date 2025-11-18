#!/usr/bin/env bash
date_str=$(date +%Y-%m-%d)
if command -v wtype >/dev/null 2>&1; then
  wtype "$date_str"
elif command -v xdotool >/dev/null 2>&1; then
  xdotool type --delay 0 "$date_str"
fi
