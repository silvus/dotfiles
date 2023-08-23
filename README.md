Personals dotfiles for Bash, Tmux, Vim, AwesomeWm, etc...

# Installation

```shell
sudo apt install fish git python3 curl tmux ssh mosh vim dfc ncdu gawk xdotool
curl -sS https://raw.githubusercontent.com/silvus/dotfiles/master/bin/dotfiles | python3
```

# Fish shell

No more fish auto start from bash, used this command instead to defined a default shell:
```shell
chsh -s $(which fish)
```
