Personals dotfiles for Bash, Tmux, Vim, AwesomeWm, etc...

# Installation

```sh
sudo apt install fish git python3 curl tmux ssh mosh vim fzf bat dfc ncdu htop btop fd-find gawk ripgrep build-essential apache2-utils xdotool command-not-found
curl -sS https://raw.githubusercontent.com/silvus/dotfiles/master/bin/dotfiles | python3
```

# Fish shell

Use this command to define `fish` has a default shell:
```sh
chsh -s $(command -v fish)
```
