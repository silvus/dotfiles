# Structure

```
nixos/
├── flake.nix                 # Main flake with dynamic host configuration
├── modules/                  # Reusable configuration modules
│   ├── base.nix             # Base system configuration for all hosts
│   ├── desktop.nix          # Desktop environment and window manager
│   ├── gaming.nix           # Gaming support and optimization
│   ├── laptop.nix           # Power management and laptop hardware
│   ├── security.nix         # Firewall, fail2ban, hardening
│   └── development.nix      # Programming languages and tools
├── profiles/                # Host profile templates
│   ├── server.nix           # Server profile (headless, security-focused)
│   ├── desktop.nix          # Desktop profile (GUI, modules enabled)
│   └── laptop.nix           # Laptop profile (extends desktop + power)
├── hosts/                   # Host-specific configurations
│   ├── nixos-vm/            # Test VM with all profiles
│   ├── noctus/              # Laptop
│   ├── somnus/              # Gaming desktop
│   ├── virtus/              # Development desktop
│   └── servius/             # Production server
└── packages/                # Legacy packages (for Debian home-manager)
```

# Commands

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

To upgrade:
``` bash
nix flake update --extra-experimental-features nix-command --extra-experimental-features flakes --flake path:/home/silvus/.dotfiles/nixos
```

If offline:
``` bash
--option substitute false
```
