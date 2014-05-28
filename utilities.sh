#!/usr/bin/env bash

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
	tmux `# Split terminal` \
	lynx `# Web browser` \
	newsbeuter `# Rss reader`

# Need Configuration
# ----------------------------------------------------------------
# gitconfig :

# Mail :
#	mutt-patched
# 	Sylpheed

# Need tests
# ----------------------------------------------------------------
# Stats
# 	GoAccess # Base on access logs, for Apache/Nginx
