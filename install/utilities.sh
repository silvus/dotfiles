#!/usr/bin/env bash

set -e

sudo apt-get update

# Installation
# ----------------------------------------------------------------
sudo apt-get install \
	curl `# Play with HTTP` \
	wget `# Downloader` \
	rsync `# Synchronisation` \
	htop `# Better top command` \
	dfc `# Disk space` \
	iftop `# Traffic htop-like` \
	build-essential `# Compiling things` \
	ack-grep `# Search` \
	vim `# Text editor` \
	xclip `# Add to clipboard` \
	scrot `# Make screenshot` \
	pass `# Passwords manager` \
	feh `# Image viewer` \
	tmux `# Split terminal` \
	lynx `# Web browser` \
	newsbeuter `# Rss reader`\
	moc `# Music player` \
	ranger highlight caca-utils `# File manager and dependencies`

