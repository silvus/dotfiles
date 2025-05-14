
To Install:
``` bash
sudo nixos-rebuild switch --flake ~/.dotfiles/nixos
```

To Upgrade:
``` bash
sudo nixos-rebuild switch --flake ~/.dotfiles/nixos --upgrade
```

To cleanup
``` bash
sudo nix-collect-garbage --delete-older-than 14d
```

To init
``` bash
nix --extra-experimental-features nix-command --extra-experimental-features flakes run nixpkgs#git -- clone https://github.com/silvus/dotfiles.git ~/.dotfiles
sudo nixos-rebuild switch --flake ~/.dotfiles/nixos --use-remote-sudo
```
