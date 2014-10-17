DOTFILES := $(shell pwd)

dotfiles:
	@bash "$(DOTFILES)/install/install.sh"

guake:
	@bash "$(DOTFILES)/install/guake-configure.sh"

desktop: dotfiles guake

