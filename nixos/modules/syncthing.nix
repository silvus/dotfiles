{ pkgs,... }:

{
  environment.systemPackages = with pkgs; [
    syncthing
  ];

  # TODO: use a user service instead?
  # TODO: Sometimes the api is not called (api key not extracted?), delete /home/silvus/.config/syncthing fix it? not really, the gui port is not updated.

  # TODO: make an external tool to configure syncthing with `syncthing cli` instead of curl commands. See https://superuser.com/questions/1397683/how-can-i-configure-syncthing-from-command-line-to-share-a-folder-with-another-c

  # https://wiki.nixos.org/wiki/Syncthing
  # https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/services/networking/syncthing.nix
  services.syncthing = {
    enable = true;
    openDefaultPorts = true; # Open ports in the firewall for Syncthing. (NOTE: this will not open syncthing gui port)

    extraFlags = [ "--no-default-folder" ]; # Don't create default ~/Sync folder

    user = "silvus";
    group = "users";

    dataDir = "/home/silvus/.local/share/syncthing";
    configDir = "/home/silvus/.config/syncthing";

    overrideDevices = true;     # overrides any devices added or deleted through the WebUI
    overrideFolders = true;     # overrides any folders added or deleted through the WebUI

    # guiAddress

    settings = {
      gui = {
        enabled = true;
        user = "myuser";
        password = "mypassword"; # TODO protect
        address = "0.0.0.0:5001";
        theme = "dark";
      };

      options = {
        urAccepted = -1;
        globalAnnounceEnabled = true;
        localAnnounceEnabled = true;
        relaysEnabled = true;
        natEnabled = true;
        startBrowser = false;
      };

      folders = {
        audiobook = {
          path = "/data/audiobooks";
          label = "audiobooks";
          type = "sendreceive";
          rescanIntervals = "9000";
          fsWatcherEnabled = false;
          devices = [ "styx" "vulcain" ];
          minDiskFree = "5GiB";
        };

        book = {
          path = "/data/books";
          label = "books";
          type = "sendreceive";
          rescanIntervals = "9500";
          fsWatcherEnabled = true;
          devices = [ "styx" "vulcain" ];
          minDiskFree = "5GiB";
        };

        dev = {
          path = "/data/dev";
          type = "sendreceive";
          rescanIntervals = "7200";
          fsWatcherEnabled = false;
          devices = [ "styx" "wxs5cxc" "w2eelgs" "zh5zam6" "vulcain" "2jl4o7y" ];
          minDiskFree = "5GiB";
        };

        doc = {
          path = "/data/doc";
          label = "doc";
          type = "sendreceive";
          rescanIntervals = "3600";
          fsWatcherEnabled = true;
          devices = [ "corus" "styx" "wxs5cxc" "w2eelgs" "zh5zam6" "vulcain" "jt3" ];
          minDiskFree = "5GiB";
        };

        image = {
          path = "/data/images";
          label = "images";
          type = "sendreceive";
          rescanIntervals = "6000";
          fsWatcherEnabled = false;
          devices = [ "styx" "vulcain" "2jl4o7y" ];
          minDiskFree = "5GiB";
        };

        music = {
          path = "/data/music";
          label = "music";
          type = "sendreceive";
          rescanIntervals = "8000";
          fsWatcherEnabled = false;
          devices = [ "vulcain" "styx" "wxs5cxc" "w2eelgs" "zh5zam6" "vulcain" "2jl4o7y" ];
          minDiskFree = "5GiB";
        };

        phone = {
          path = "/data/phone";
          label = "phone";
          type = "sendreceive";
          rescanIntervals = "9000";
          fsWatcherEnabled = false;
          devices = [ "corus" "styx" "wxs5cxc" "zh5zam6" "vulcain" "2jl4o7y" ];
          minDiskFree = "5GiB";
        };

        photo = {
          path = "/data/photos";
          label = "photos";
          type = "sendreceive";
          rescanIntervals = "7500";
          fsWatcherEnabled = false;
          devices = [ "styx" "vulcain" "2jl4o7y" ];
          minDiskFree = "5GiB";
        };

        pixel_5_tcbd_photos = {
          path = "/data/phone_photos";
          label = "phone_photos";
          type = "sendreceive";
          rescanIntervals = "4000";
          fsWatcherEnabled = false;
          devices = [ "corus" "styx" "vulcain" ];
          minDiskFree = "5GiB";
        };

        share = {
          path = "/data/prod/share";
          label = "share";
          type = "sendreceive";
          rescanIntervals = "72000";
          fsWatcherEnabled = false;
          devices = [ "styx" "wxs5cxc" "zh5zam6" "vulcain" "2jl4o7y" "jt3" ];
          minDiskFree = "5GiB";
        };

        tiago = {
          path = "/data/tiago";
          label = "tiago";
          type = "sendreceive";
          rescanIntervals = "8000";
          fsWatcherEnabled = false;
          devices = [ "vulcain" "chuwi" "styx" "r2s5alz" "vulcain" ];
          minDiskFree = "5GiB";
        };

        tiago_devoirs = {
          path = "/data/tiago_devoirs";
          label = "tiago_devoirs";
          type = "sendreceive";
          rescanIntervals = "8000";
          fsWatcherEnabled = true;
          devices = [ "chuwi" "styx" "vulcain" ];
          minDiskFree = "5GiB";
        };

        video = {
          path = "/data/videos";
          label = "videos";
          type = "sendreceive";
          rescanIntervals = "6500";
          fsWatcherEnabled = false;
          devices = [ "styx" "vulcain" "2jl4o7y" ];
          minDiskFree = "5GiB";
        };

        work = {
          path = "/data/work";
          label = "work";
          type = "sendreceive";
          rescanIntervals = "6300";
          fsWatcherEnabled = true;
          devices = [ "seven" "styx" "wxs5cxc" "zh5zam6" "vulcain" "2jl4o7y" "jt3" "seven" ];
          minDiskFree = "5GiB";
        };

        z = {
          path = "/data/z";
          label = "z";
          type = "sendreceive";
          rescanIntervals = "7300";
          fsWatcherEnabled = false;
          devices = [ "styx" "vulcain" "2jl4o7y" ];
          minDiskFree = "5GiB";
        };
      };

      devices = {
        styx = {
          id = "FGBN3T7-L6Q6QOR-SBYPVHS-3MWONAJ-REDACTED";
          name = "styx";
          introducer = false;
          compression = "metadata";
          addresses = [ "dynamic" ];
        };

        vulcain = {
          id = "GVRLL7S-3XQUVNR-57JFCXW-XHF25FQ-L6RPLR3-REDACTED";
          name = "vulcain";
          introducer = false;
          compression = "metadata";
          addresses = [ "dynamic" ];
        };

        corus = {
          id = "HIG52YZ-X2B46F3-ZDEQT2H-HY6SN3D-JUWGRVA-REDACTED";
          name = "corus";
          introducer = false;
          compression = "metadata";
          addresses = [ "dynamic" ];
        };

        chuwi = {
          id = "ICSTELQ-FSFVTT4-NFHZ2RD-SWO22XS-REDACTED";
          name = "chuwi";
          introducer = false;
          compression = "metadata";
          addresses = [ "dynamic" ];
        };

        r2s5alz = {
          id = "R2S5ALZ-A4RFZK5-ADC2PGR-24Y6PUF-REDACTED";
          name = "r2s5alz";
          introducer = false;
          compression = "metadata";
          addresses = [ "dynamic" ];
        };

        jt3 = {
          id = "3JT3QIL-KJ7F3WQ-HHS2A2B-OXPTAP3-REDACTED";
          name = "jt3";
          introducer = false;
          compression = "metadata";
          addresses = [ "dynamic" ];
        };

        seven = {
          id = "7GVTWHS-UZILMDL-ER4YDMR-SYUVX33-REDACTED";
          name = "seven";
          introducer = false;
          compression = "metadata";
          addresses = [ "dynamic" ];
        };

        zh5zam6 = {
          id = "ZH5ZAM6-W5RSIXN-4M27KNK-OCKETN3-REDACTED";
          name = "zh5zam6";
          introducer = false;
          compression = "metadata";
          addresses = [ "dynamic" ];
        };

        wxs5cxc = {
          id = "WXS5CXC-V5M3NOY-IHK6Z6R-33VJNVH-REDACTED";
          name = "wxs5cxc";
          introducer = false;
          compression = "metadata";
          addresses = [ "dynamic" ];
        };

        w2eelgs = {
          id = "W2EELGS-LCFCSNK-GZIBZUK-ZOOF6KI-GRH2Q3T-REDACTED";
          name = "w2eelgs";
          introducer = false;
          compression = "metadata";
          addresses = [ "dynamic" ];
        };

        "2jl4o7y" = {
          id = "JEQKQZ3-B5YABIT-SDD7ZV4-EEX5S5M-REDACTED";
          name = "2jl4o7y";
          introducer = false;
          compression = "metadata";
          addresses = [ "dynamic" ];
        };
      };

    };
  };

  # networking.firewall.allowedTCPPorts = [ 8384 22000 ];
  # networking.firewall.allowedUDPPorts = [ 21027 22000 ];

  # systemd.tmpfiles.rules =
  #   [
  #     "d /home/silvus/.local/share/syncthing 0755 silvus users -"
  #     "d /home/silvus/.config/syncthing 0755 silvus users -"
  #     "d /data 0755 silvus users -"
  #   ]
  #   ++ map (n: "d /data/${n} 0755 silvus users -") sharedFolders;
}
