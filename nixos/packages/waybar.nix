{ pkgs, ... }:

{

  home.packages = with pkgs; [
    waybar

    # gtk css interfer and force a with of 28px (which is huge)
    # to force a small size, fake a smaller gtk theme
    adapta-gtk-theme
  ];

  programs.waybar = {
    enable = true;
    # package = pkgs.waybar.overrideAttrs (oa: {
    #   mesonFlags = (oa.mesonFlags or []) ++ ["-Dexperimental=true"];
    # });
    # systemd.enable = true;
    settings = {
      primary = {
        # exclusive = false;
        # passthrough = false;
        # height = 19;
        width = 19;
        # margin = "6";
        # "spacing" = 4; # Gaps between modules

        position = "left";
        modules-left = [
          "sway/workspaces"
          # "sway/window"
        ];

        # modules-center = [
        #   "cpu"
        #   "memory"
        # ];

        modules-right = [
          "tray"
          "network"
          "pulseaudio"
          "battery"
          "battery#bat2"
          "clock"
        ];

        clock = {
          rotate = 90;
          interval = 1;
          format = "{:%H:%M:%S}";
          # format-alt = "{:%Y-%m-%d %H:%M:%S %z}";
          # on-click-left = "mode";
          tooltip-format = ''
            <big>{:%Y %B}</big>
            <tt><small>{calendar}</small></tt>'';
        };

        "sway/workspaces" = {
          rotate = 90;
        };
        #  "sway/window" = {
        #   rotate = 90;
        # };

        # cpu = {
        #   rotate = 90;
        #   # format = "  {usage}%";
        # };
        # memory = {
        #   rotate = 90;
        #   # format = "  {}%";
        #   # interval = 5;
        # };

        pulseaudio = {
          rotate = 90;
        };

        # # pulseaudio = {
        # #   format-source = "󰍬 {volume}%";
        # #   format-source-muted = "󰍭 0%";
        # #   format = "{icon} {volume}% {format_source}";
        # #   format-muted = "󰸈 0% {format_source}";
        # #   format-icons = {
        # #     default = [
        # #       "󰕿"
        # #       "󰖀"
        # #       "󰕾"
        # #     ];
        # #   };
        # #   on-click = lib.getExe pkgs.pavucontrol;
        # # };
        # idle_inhibitor = {
        #   format = "{icon}";
        #   format-icons = {
        #     activated = "󰒳";
        #     deactivated = "󰒲";
        #   };
        # };

        battery = {
          "format" = "{icon}";
          # "rotate": 0,
          "format-charging" = "<span color='#a6d189'>󱐋</span>";
          "format-plugged" = "󰂄";
          "format-icons" = [
            "󰝦"
            "󰪞"
            "󰪟"
            "󰪠"
            "󰪡"
            "󰪢"
            "󰪣"
            "󰪤"
            "󰪥"
          ];
        };
        "battery#bat2" = {
          "format" = "{icon}";
          # "rotate": 0,
          "format-charging" = "<span color='#a6d189'>󱐋</span>";
          "format-plugged" = "󰂄";
          "format-icons" = [
            "󰝦"
            "󰪞"
            "󰪟"
            "󰪠"
            "󰪡"
            "󰪢"
            "󰪣"
            "󰪤"
            "󰪥"
          ];
        };

        # battery = {
        #   bat = "BAT0";
        #   interval = 10;
        #   format-icons = [
        #     "󰁺"
        #     "󰁻"
        #     "󰁼"
        #     "󰁽"
        #     "󰁾"
        #     "󰁿"
        #     "󰂀"
        #     "󰂁"
        #     "󰂂"
        #     "󰁹"
        #   ];
        #   format = "{icon} {capacity}%";
        #   format-charging = "󰂄 {capacity}%";
        #   onclick = "";
        # };
        network = {
          rotate = 90;
          interval = 3;
          format-wifi = "   {essid}";
          format-ethernet = "󰈁 Connected";
          format-disconnected = "";
          tooltip-format = ''
            {ifname}
            {ipaddr}/{cidr}
            Up: {bandwidthUpBits}
            Down: {bandwidthDownBits}'';
        };
      };
    };

    style = ''
  * {
    border: none;
    border-radius: 0;
    padding: 0;
    margin: 0;
    min-height: 0;
    font-family: "JetBrainsMono Nerd Font", monospace;
    font-size: 13px;
    background: transparent;
    color: #d3c6aa;
  }
  window#waybar {
    background: #272e33;
    color: #d3c6aa;
  }
  #workspaces button {
    padding: 4px 1px;
    margin: 5px 0;
    background: #2e383c;
    color: #859289;
    border: none;
  }
  #workspaces button.focused {
    background: #a7c080;
    color: #000000;
    font-weight: bold;
  }
  #tray,
  #network,
  #pulseaudio,
  #battery,
  #battery#bat2,
  #clock {
    padding: 4px 1px;
    margin: 5px 0;
    background: #2e383c;
    color: #d3c6aa;
  }
    '';
  };

}

