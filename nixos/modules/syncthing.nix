{ pkgs,... }:

{
  environment.systemPackages = with pkgs; [
    syncthing
  ];

  # TODO: use a user service instead?

  # https://wiki.nixos.org/wiki/Syncthing
  services.syncthing = {
    enable = true;
    openDefaultPorts = true; # Open ports in the firewall for Syncthing. (NOTE: this will not open syncthing gui port)

    extraFlags = [ "--no-default-folder" ]; # Don't create default ~/Sync folder

    user = "silvus";
    dataDir = "/home/silvus/.local/share/syncthing";
    configDir = "/home/silvus/.config/syncthing";

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
          rescanIntervalS = 9000;
          fsWatcherEnabled = false;
          devices = [
            "JEQKQZ3-B5YABIT-SDD7ZV4-EEX5S5M-REDACTED"
            "ZI67LFV-DU2Z4U3-5XXXHXX-SDJHTQJ-REDACTED"
          ];
          minDiskFree = "5GiB";
        };

        book = {
          path = "/data/books";
          label = "books";
          type = "sendreceive";
          rescanIntervalS = 9500;
          fsWatcherEnabled = true;
          devices = [
            "JEQKQZ3-B5YABIT-SDD7ZV4-EEX5S5M-REDACTED"
            "ZI67LFV-DU2Z4U3-5XXXHXX-SDJHTQJ-REDACTED"
          ];
          minDiskFree = "5GiB";
        };

        dev = {
          path = "/data/dev";
          type = "sendreceive";
          rescanIntervalS = 7200;
          fsWatcherEnabled = false;
          devices = [
            "JEQKQZ3-B5YABIT-SDD7ZV4-EEX5S5M-REDACTED"
            "WXS5CXC-V5M3NOY-IHK6Z6R-33VJNVH-REDACTED"
            "W2EELGS-LCFCSNK-GZIBZUK-ZOOF6KI-GRH2Q3T-REDACTED"
            "ZH5ZAM6-W5RSIXN-4M27KNK-OCKETN3-REDACTED"
            "ZI67LFV-DU2Z4U3-5XXXHXX-SDJHTQJ-REDACTED"
            "2JL4O7Y-IGMULRC-MM4O627-JLIZ7WX-REDACTED"
          ];
          minDiskFree = "5GiB";
        };

        doc = {
          path = "/data/doc";
          label = "doc";
          type = "sendreceive";
          rescanIntervalS = 3600;
          fsWatcherEnabled = true;
          devices = [
            "HIG52YZ-X2B46F3-ZDEQT2H-HY6SN3D-JUWGRVA-REDACTED"
            "JEQKQZ3-B5YABIT-SDD7ZV4-EEX5S5M-REDACTED"
            "WXS5CXC-V5M3NOY-IHK6Z6R-33VJNVH-REDACTED"
            "W2EELGS-LCFCSNK-GZIBZUK-ZOOF6KI-GRH2Q3T-REDACTED"
            "ZH5ZAM6-W5RSIXN-4M27KNK-OCKETN3-REDACTED"
            "ZI67LFV-DU2Z4U3-5XXXHXX-SDJHTQJ-REDACTED"
            "2JL4O7Y-IGMULRC-MM4O627-JLIZ7WX-REDACTED"
            "3JT3QIL-KJ7F3WQ-HHS2A2B-OXPTAP3-REDACTED"
          ];
          minDiskFree = "5GiB";
        };

        image = {
          path = "/data/images";
          label = "images";
          type = "sendreceive";
          rescanIntervalS = 6000;
          fsWatcherEnabled = false;
          devices = [
            "JEQKQZ3-B5YABIT-SDD7ZV4-EEX5S5M-REDACTED"
            "ZI67LFV-DU2Z4U3-5XXXHXX-SDJHTQJ-REDACTED"
            "2JL4O7Y-IGMULRC-MM4O627-JLIZ7WX-REDACTED"
          ];
          minDiskFree = "5GiB";
        };

        music = {
          path = "/data/music";
          label = "music";
          type = "sendreceive";
          rescanIntervalS = 8000;
          fsWatcherEnabled = false;
          devices = [
            "GVRLL7S-3XQUVNR-57JFCXW-XHF25FQ-L6RPLR3-REDACTED"
            "JEQKQZ3-B5YABIT-SDD7ZV4-EEX5S5M-REDACTED"
            "WXS5CXC-V5M3NOY-IHK6Z6R-33VJNVH-REDACTED"
            "W2EELGS-LCFCSNK-GZIBZUK-ZOOF6KI-GRH2Q3T-REDACTED"
            "ZH5ZAM6-W5RSIXN-4M27KNK-OCKETN3-REDACTED"
            "ZI67LFV-DU2Z4U3-5XXXHXX-SDJHTQJ-REDACTED"
            "2JL4O7Y-IGMULRC-MM4O627-JLIZ7WX-REDACTED"
          ];
          minDiskFree = "5GiB";
        };

        phone = {
          path = "/data/phone";
          label = "phone";
          type = "sendreceive";
          rescanIntervalS = 9000;
          fsWatcherEnabled = false;
          devices = [
            "HIG52YZ-X2B46F3-ZDEQT2H-HY6SN3D-JUWGRVA-REDACTED"
            "JEQKQZ3-B5YABIT-SDD7ZV4-EEX5S5M-REDACTED"
            "WXS5CXC-V5M3NOY-IHK6Z6R-33VJNVH-REDACTED"
            "ZH5ZAM6-W5RSIXN-4M27KNK-OCKETN3-REDACTED"
            "ZI67LFV-DU2Z4U3-5XXXHXX-SDJHTQJ-REDACTED"
            "2JL4O7Y-IGMULRC-MM4O627-JLIZ7WX-REDACTED"
          ];
          minDiskFree = "5GiB";
        };

        photo = {
          path = "/data/photos";
          label = "photos";
          type = "sendreceive";
          rescanIntervalS = 7500;
          fsWatcherEnabled = false;
          devices = [
            "JEQKQZ3-B5YABIT-SDD7ZV4-EEX5S5M-REDACTED"
            "ZI67LFV-DU2Z4U3-5XXXHXX-SDJHTQJ-REDACTED"
            "2JL4O7Y-IGMULRC-MM4O627-JLIZ7WX-REDACTED"
          ];
          minDiskFree = "5GiB";
        };

        pixel_5_tcbd_photos = {
          path = "/data/phone_photos";
          label = "phone_photos";
          type = "sendreceive";
          rescanIntervalS = 4000;
          fsWatcherEnabled = false;
          devices = [
            "HIG52YZ-X2B46F3-ZDEQT2H-HY6SN3D-JUWGRVA-REDACTED"
            "JEQKQZ3-B5YABIT-SDD7ZV4-EEX5S5M-REDACTED"
            "ZI67LFV-DU2Z4U3-5XXXHXX-SDJHTQJ-REDACTED"
          ];
          minDiskFree = "5GiB";
        };

        share = {
          path = "/data/prod/share";
          label = "share";
          type = "sendreceive";
          rescanIntervalS = 72000;
          fsWatcherEnabled = false;
          devices = [
            "JEQKQZ3-B5YABIT-SDD7ZV4-EEX5S5M-REDACTED"
            "WXS5CXC-V5M3NOY-IHK6Z6R-33VJNVH-REDACTED"
            "ZH5ZAM6-W5RSIXN-4M27KNK-OCKETN3-REDACTED"
            "ZI67LFV-DU2Z4U3-5XXXHXX-SDJHTQJ-REDACTED"
            "2JL4O7Y-IGMULRC-MM4O627-JLIZ7WX-REDACTED"
            "3JT3QIL-KJ7F3WQ-HHS2A2B-OXPTAP3-REDACTED"
          ];
          minDiskFree = "5GiB";
        };

        trogo = {
          path = "/data/trogo";
          label = "trogo";
          type = "sendreceive";
          rescanIntervalS = 8000;
          fsWatcherEnabled = false;
          devices = [
            "GVRLL7S-3XQUVNR-57JFCXW-XHF25FQ-L6RPLR3-REDACTED"
            "ICSTELQ-FSFVTT4-NFHZ2RD-SWO22XS-REDACTED"
            "JEQKQZ3-B5YABIT-SDD7ZV4-EEX5S5M-REDACTED"
            "R2S5ALZ-A4RFZK5-ADC2PGR-24Y6PUF-REDACTED"
            "ZI67LFV-DU2Z4U3-5XXXHXX-SDJHTQJ-REDACTED"
          ];
          minDiskFree = "5GiB";
        };

        trogo_devoirs = {
          path = "/data/trogo_devoirs";
          label = "trogo_devoirs";
          type = "sendreceive";
          rescanIntervalS = 8000;
          fsWatcherEnabled = true;
          devices = [
            "ICSTELQ-FSFVTT4-NFHZ2RD-SWO22XS-REDACTED"
            "JEQKQZ3-B5YABIT-SDD7ZV4-EEX5S5M-REDACTED"
            "ZI67LFV-DU2Z4U3-5XXXHXX-SDJHTQJ-REDACTED"
          ];
          minDiskFree = "5GiB";
        };

        video = {
          path = "/data/videos";
          label = "videos";
          type = "sendreceive";
          rescanIntervalS = 6500;
          fsWatcherEnabled = false;
          devices = [
            "JEQKQZ3-B5YABIT-SDD7ZV4-EEX5S5M-REDACTED"
            "ZI67LFV-DU2Z4U3-5XXXHXX-SDJHTQJ-REDACTED"
            "2JL4O7Y-IGMULRC-MM4O627-JLIZ7WX-REDACTED"
          ];
          minDiskFree = "5GiB";
        };

        work = {
          path = "/data/work";
          label = "work";
          type = "sendreceive";
          rescanIntervalS = 6300;
          fsWatcherEnabled = true;
          devices = [
            "FGBN3T7-L6Q6QOR-SBYPVHS-3MWONAJ-REDACTED"
            "JEQKQZ3-B5YABIT-SDD7ZV4-EEX5S5M-REDACTED"
            "WXS5CXC-V5M3NOY-IHK6Z6R-33VJNVH-REDACTED"
            "ZH5ZAM6-W5RSIXN-4M27KNK-OCKETN3-REDACTED"
            "ZI67LFV-DU2Z4U3-5XXXHXX-SDJHTQJ-REDACTED"
            "2JL4O7Y-IGMULRC-MM4O627-JLIZ7WX-REDACTED"
            "3JT3QIL-KJ7F3WQ-HHS2A2B-OXPTAP3-REDACTED"
            "7GVTWHS-UZILMDL-ER4YDMR-SYUVX33-REDACTED"
          ];
          minDiskFree = "5GiB";
        };

        z = {
          path = "/data/z";
          label = "z";
          type = "sendreceive";
          rescanIntervalS = 7300;
          fsWatcherEnabled = false;
          devices = [
            "JEQKQZ3-B5YABIT-SDD7ZV4-EEX5S5M-REDACTED"
            "ZI67LFV-DU2Z4U3-5XXXHXX-SDJHTQJ-REDACTED"
            "2JL4O7Y-IGMULRC-MM4O627-JLIZ7WX-REDACTED"
          ];
          minDiskFree = "5GiB";
        };
      };

      devices = {
        styx = {
          deviceId = "FGBN3T7-L6Q6QOR-SBYPVHS-3MWONAJ-REDACTED";
          name = "styx";
          introducer = false;
          compression = "metadata";
          addresses = [ "dynamic" ];
        };

        vulcain = {
          deviceId = "GVRLL7S-3XQUVNR-57JFCXW-XHF25FQ-L6RPLR3-REDACTED";
          name = "vulcain";
          introducer = false;
          compression = "metadata";
          addresses = [ "dynamic" ];
        };

        corus = {
          deviceId = "HIG52YZ-X2B46F3-ZDEQT2H-HY6SN3D-JUWGRVA-REDACTED";
          name = "corus";
          introducer = false;
          compression = "metadata";
          addresses = [ "dynamic" ];
        };

        chuwi = {
          deviceId = "ICSTELQ-FSFVTT4-NFHZ2RD-SWO22XS-REDACTED";
          name = "chuwi";
          introducer = false;
          compression = "metadata";
          addresses = [ "dynamic" ];
        };

        r2s5alz = {
          deviceId = "R2S5ALZ-A4RFZK5-ADC2PGR-24Y6PUF-REDACTED";
          name = "r2s5alz";
          introducer = false;
          compression = "metadata";
          addresses = [ "dynamic" ];
        };

        jt3 = {
          deviceId = "3JT3QIL-KJ7F3WQ-HHS2A2B-OXPTAP3-REDACTED";
          name = "jt3";
          introducer = false;
          compression = "metadata";
          addresses = [ "dynamic" ];
        };

        seven = {
          deviceId = "7GVTWHS-UZILMDL-ER4YDMR-SYUVX33-REDACTED";
          name = "seven";
          introducer = false;
          compression = "metadata";
          addresses = [ "dynamic" ];
        };

        zh5zam6 = {
          deviceId = "ZH5ZAM6-W5RSIXN-4M27KNK-OCKETN3-REDACTED";
          name = "zh5zam6";
          introducer = false;
          compression = "metadata";
          addresses = [ "dynamic" ];
        };

        wxs5cxc = {
          deviceId = "WXS5CXC-V5M3NOY-IHK6Z6R-33VJNVH-REDACTED";
          name = "wxs5cxc";
          introducer = false;
          compression = "metadata";
          addresses = [ "dynamic" ];
        };

        w2eelgs = {
          deviceId = "W2EELGS-LCFCSNK-GZIBZUK-ZOOF6KI-GRH2Q3T-REDACTED";
          name = "w2eelgs";
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
