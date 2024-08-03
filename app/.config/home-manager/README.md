```sh
curl -L https://nixos.org/nix/install | sh
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
nix-channel --update
home-manager switch
```
