``` bash
sudo nixos-rebuild --upgrade -I nixos-config=$HOME/.dotfiles/nixos/configuration.nix switch
```

To Upgrade:
``` bash
sudo nix-channel --update; sudo systemctl daemon-reload; sudo systemctl restart nix-daemon
```
