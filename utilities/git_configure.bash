#!/usr/bin/env bash

# Configure git
# -----------------------------------------------------------------------------
# git config --global user.name "silvus"
# git config --global user.email "silvus@MAIL.com"

git config --global color.ui "auto"

git config --global core.editor "vim"
git config --global core.autocrlf "input"

git config --global alias.st "status"
git config --global alias.aa "add --all"
git config --global alias.co "commit -av"
git config --global alias.gr "grep -p"
git config --global alias.pusom "push origin master"
git config --global alias.pulom "pull origin master"
git config --global alias.dif "diff --stat=160,120"
git config --global alias.lg "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --"

git config --global web.browser "lynx"

git config --global diff.tool "vimdiff"
git config --global difftool.prompt false

git config --global github.user "Silvus"
git config --global credential.helper "cache --timeout=3600"

git config --global core.excludesfile "~/.gitignore_global"
