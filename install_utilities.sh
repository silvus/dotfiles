#!/usr/bin/env bash

OS_VERSION_NAME=$(lsb_release -cs)

sudo apt-get update
sudo apt-get install \
	python-software-properties `# Add PPA`

echo $OS_VERSION_NAME

# Ubuntu 14.04 specific
# ----------------------------------------------------------------
if [ $OS_VERSION_NAME = 'trusty' ]; then
	sudo apt-get install \
		dfc `# Disk space`
fi

# Ubuntu 12.04 specific
# ----------------------------------------------------------------
if [ $OS_VERSION_NAME = 'precise' ]; then
	sudo add-apt-repository ppa:kalakris/tmux
fi

# Installation
# ----------------------------------------------------------------
sudo apt-get install \
	vim `# Text editor` \
	htop `# Better "top" command` \
	curl `# Play with HTTP` \
	wget `# Downloader` \
	dfc `# Disk space` \
	rsync `# Synchronisation` \
	build-essential `# Compiling things` \
	ack-grep `# Search` \
	tmux `# Split terminal`


# Draft
# ----------------------------------------------------------------
# Rss
# 	newsbeuter
# Mail
# 	mutt
