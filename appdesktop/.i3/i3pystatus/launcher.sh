#!/usr/bin/env bash

cd ~/.i3
export PYTHONPATH="./.pip:$PYTHONPATH"

python3 ~/.i3/i3pystatus/bar.py
