
To Install:
``` bash
sudo nixos-rebuild switch --flake path:/home/silvus/.dotfiles/nixos
```
Note: `path:` is used to copy everything to nix store regardless to the git status.
See https://github.com/NixOS/nix/issues/7107#issuecomment-2002363048


To Upgrade:
``` bash
sudo nixos-rebuild switch --flake path:/home/silvus/.dotfiles/nixos --upgrade
```

To cleanup
``` bash
sudo nix-collect-garbage --delete-older-than 14d
```

To init
``` bash
useradd -m -G wheel -s /run/current-system/sw/bin/bash silvus
passwd silvus
su - silvus
nix --extra-experimental-features nix-command --extra-experimental-features flakes run nixpkgs#git -- clone https://github.com/silvus/dotfiles.git ~/.dotfiles
sudo nixos-rebuild switch --flake path:/home/silvus/.dotfiles/nixos#the-hostname --use-remote-sudo
```

On Debian, install Nix and Homemanager (standalone), then:
``` bash
home-manager switch --extra-experimental-features nix-command --extra-experimental-features flakes --flake path:/home/silvus/.dotfiles/nixos#silvus
```
