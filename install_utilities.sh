#!/usr/bin/env bash

OS_VERSION_NAME=$(lsb_release -cs)

sudo apt-get update

# Ubuntu 14.04 specific
# ----------------------------------------------------------------
if [ $OS_VERSION_NAME = 'trusty' ]; then
	sudo apt-get install \
		dfc `# Disk space` \
		iftop `# htop for net traffic`
fi

# Ubuntu 12.04 specific
# ----------------------------------------------------------------
if [ $OS_VERSION_NAME = 'precise' ]; then
	# Add PPA for tmux 1.8
	sudo apt-get install python-software-properties
	sudo add-apt-repository ppa:kalakris/tmux
fi

# Installation
# ----------------------------------------------------------------
sudo apt-get install \
	vim `# Text editor` \
	htop `# Better "top" command` \
	curl `# Play with HTTP` \
	wget `# Downloader` \
	rsync `# Synchronisation` \
	build-essential `# Compiling things` \
	ack-grep `# Search` \
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
