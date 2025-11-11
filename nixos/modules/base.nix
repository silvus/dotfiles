{ pkgs, ... }:

{
  # Base system configuration shared by all hosts

  # Bootloader
  boot.loader = {
    timeout = 1;
    efi.canTouchEfiVariables = true;
    systemd-boot = {
      enable = true;
      configurationLimit = 9;
    };
  };

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone
  time.timeZone = "Europe/Paris";

  # Internationalization
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "fr_FR.UTF-8";
      LC_IDENTIFICATION = "fr_FR.UTF-8";
      LC_MEASUREMENT = "fr_FR.UTF-8";
      LC_MONETARY = "fr_FR.UTF-8";
      LC_NAME = "fr_FR.UTF-8";
      LC_NUMERIC = "fr_FR.UTF-8";
      LC_PAPER = "fr_FR.UTF-8";
      LC_TELEPHONE = "fr_FR.UTF-8";
      LC_TIME = "fr_FR.UTF-8";
      LC_CTYPE = "fr_FR.UTF8";
      LC_MESSAGES = "fr_FR.UTF-8";
      LC_COLLATE = "fr_FR.UTF-8";
    };
  };

  # Console keymap
  console = {
    # keyMap = "us";
    # Derive from X11 value
    useXkbConfig = true;
  };

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Nix configuration
  nix = {
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      warn-dirty = false;
      auto-optimise-store = true;
      # trusted-users = [ "root" "@wheel" ];
    };

    # Garbage collection
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };

  # Define user account
  users.users.silvus = {
    isNormalUser = true;
    description = "Silvus";
    extraGroups = [
      "networkmanager"
      "wheel"
      "audio"
      "video"
      "storage"
      "optical"
      "scanner"
      "lp"
    ];
    shell = pkgs.fish;
  };

  # Base system packages (minimal, essential only)
  environment.systemPackages = with pkgs; [
    # Editor
    vim
    neovim
    nano
    helix

    # Network
    wget
    curl
    mosh
    syncthing
    keyd

    # Shell and terminal tools
    fish
    tmux

    # Basic utilities
    which
    most
    file
    git
    fzf
    bat
    fd
    jq
    gawk
    unzip
    unrar

    # Scripts
    python3

    # Security
    gnupg

    # System information
    ncdu
    dfc
    htop
    btop

  ];

  # Enable essential programs
  programs = {
    vim = {
      enable = true;
      defaultEditor = true;
    };
    fish.enable = true;
    git.enable = true;
  };
  environment.variables.EDITOR = "vim";

  # Set default shell
  environment.shells = with pkgs; [ fish bash ];

  # Enable OpenSSH daemon
  services.openssh = {
    enable = true;
    # settings = {
    #   PasswordAuthentication = false;
    #   PermitRootLogin = "no";
    #   X11Forwarding = false;
    # };
    # openFirewall = true;
  };

  # Enable Syncthing service
  # https://wiki.nixos.org/wiki/Syncthing
  # https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/services/networking/syncthing.nix
  # Setttings are handled by a curl command who failed silently... maybe it doesn't get the api key from the config xml.
  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    user = "silvus";
    group = "users";
    dataDir = "/home/silvus/.local/share/syncthing";
    configDir = "/home/silvus/.config/syncthing";
    guiAddress = "0.0.0.0:5001";
  };

  # Open Syncthing ports in firewall
  networking.firewall = {
    allowedTCPPorts = [ 22000 ];
    allowedUDPPorts = [ 21027 22000 ];
  };

  # Configure keyd for keyboard remapping (converted from Kanata)
  # https://wiki.nixos.org/wiki/Keyd
  services.keyd = {
    enable = true;
    keyboards = {
      # The name is just the name of the configuration file, it does not really matter
      default = {
        ids = [ "*" ]; # what goes into the [id] section, here we select all keyboards
        settings = {
          main = {
            # Auto-shift behavior: tap for normal key, hold for shifted version
            "1" = "overload(shift, 1)";
            "2" = "overload(shift, 2)";
            "3" = "overload(shift, 3)";
            "4" = "overload(shift, 4)";
            "5" = "overload(shift, 5)";
            "6" = "overload(shift, 6)";
            "7" = "overload(shift, 7)";
            "8" = "overload(shift, 8)";
            "9" = "overload(shift, 9)";
            "0" = "overload(shift, 0)";
            "-" = "overload(shift, -)";
            "=" = "overload(shift, =)";

            "tab" = "overload(shift, tab)";
            "q" = "overload(shift, q)";
            "w" = "overload(shift, w)";
            "e" = "overload(shift, e)";
            "r" = "overload(shift, r)";
            "t" = "overload(shift, t)";
            "y" = "overload(shift, y)";
            "u" = "overload(shift, u)";
            "i" = "overload(shift, i)";
            "o" = "overload(shift, o)";
            "p" = "overload(shift, p)";
            "[" = "overload(shift, [)";
            "]" = "overload(shift, ])";
            "\\" = "overload(shift, \\)";

            "a" = "overload(shift, a)";
            "s" = "overload(shift, s)";
            "d" = "overload(shift, d)";
            "f" = "overload(shift, f)";
            "g" = "overload(shift, g)";
            "h" = "overload(shift, h)";
            "j" = "overload(shift, j)";
            "k" = "overload(shift, k)";
            "l" = "overload(shift, l)";
            ";" = "overload(shift, ;)";
            "'" = "overload(shift, ')";

            "z" = "overload(shift, z)";
            "x" = "overload(shift, x)";
            "c" = "overload(shift, c)";
            "v" = "overload(shift, v)";
            "b" = "overload(shift, b)";
            "n" = "overload(shift, n)";
            "m" = "overload(shift, m)";
            "," = "overload(shift, ,)";
            "." = "overload(shift, .)";
            "/" = "overload(shift, /)";

            # Special key mappings
            "grave" = "overload(media, grave)";  # tap: grave, hold: media layer
            "space" = "overload(nav, space)";    # tap: space, hold: nav layer
            "capslock" = "overload(nav, esc)";   # tap: esc, hold: nav layer
          };

          # Navigation layer (activated by holding space or capslock)
          nav = {
            "q" = "nop";
            "w" = "nop";
            "e" = "nop";
            "r" = "nop";
            "t" = "nop";
            "y" = "pageup";
            "u" = "home";
            "i" = "up";
            "o" = "end";
            "p" = "backspace";

            "a" = "nop";
            "s" = "nop";
            "d" = "nop";
            "f" = "nop";
            "g" = "nop";
            "h" = "pagedown";
            "j" = "left";
            "k" = "down";
            "l" = "right";
            ";" = "enter";
            "'" = "enter";
          };

          # Media layer (activated by holding grave)
          media = {
            "q" = "playpause";
            "w" = "previoussong";
            "e" = "nextsong";

            "z" = "volumedown";
            "x" = "mute";
            "c" = "volumeup";
          };
        };
      };
    };
  };

  # Basic security settings
  # security = {
  #   sudo = {
  #     enable = true;
  #     wheelNeedsPassword = true;
  #   };
  #   polkit.enable = true;
  # };

  # System state version
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05";
}
