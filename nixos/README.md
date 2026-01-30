# NixOs

To Install:
``` bash
sudo nixos-rebuild switch --flake path:/home/silvus/.dotfiles --sudo
```
Note: `path:` is used to copy everything to nix store regardless to the git status.
See https://github.com/NixOS/nix/issues/7107#issuecomment-2002363048


To Upgrade:
``` bash
sudo nixos-rebuild switch --flake path:/home/silvus/.dotfiles --upgrade
```

To cleanup
``` bash
sudo nix-collect-garbage --delete-older-than 14d
```

To init
``` bash
nixos-rebuild switch --flake github:silvus/dotfiles#nixos-vm
passwd silvus
su - silvus
curl -sS https://raw.githubusercontent.com/silvus/dotfiles/master/bin/dotfiles | python3
```

# Nix
On Debian, install Nix and Homemanager (standalone), then:
``` bash
home-manager switch --extra-experimental-features nix-command --extra-experimental-features flakes --flake path:/home/silvus/.dotfiles#silvus
```

To upgrade:
``` bash
nix flake update --extra-experimental-features nix-command --extra-experimental-features flakes --flake path:/home/silvus/.dotfiles
```

If offline:
``` bash
--option substitute false
```
