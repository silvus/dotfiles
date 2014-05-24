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
# Mail
# 	mutt

# Need tests
# ----------------------------------------------------------------
# Stats
# 	GoAccess # Base on access logs, for Apache/Nginx

# Twitter
# 	ttytter (perl : http://www.floodgap.com/software/ttytter/dist2/2.1.00.txt)
#	turses (python : https://github.com/alejandrogomez/turses)
