#!/usr/bin/env bash

OS_VERSION_NAME=$(lsb_release -cs)

sudo apt-get update
sudo apt-get install \
	python-software-properties `# Add PPA`

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


# Need tests
# ----------------------------------------------------------------
# Rss
# 	newsbeuter (http://synflood.at/newsbeuter/newsbeuter.html)

# Mail
# 	mutt

# Music
# 	MOC - music on console
# 	cmus
# 	Mpg123

# Web
# 	lynx

# File
# 	ranger

# Stats
# 	GoAccess # Base on access logs, for Apache/Nginx
# 	cloc # Stats for code

# Scan
# 	mtr # traceroute
# 	nmap # Ports scan
# 	netstat # Ports scan

# Twitter
# 	ttytter
#	turses
#	earthquake
