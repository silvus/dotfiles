# To install
```sh
curl -L https://nixos.org/nix/install | sh
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
nix-channel --update
nix-shell '<home-manager>' -A install

home-manager switch
```

# To update
```sh
nix-channel --update && home-manager switch
```
