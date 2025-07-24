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
          "battery"
          "battery#bat2"
          "pulseaudio/slider"
          "clock"
        ];

        clock = {
          rotate = 90;
          interval = 1;
          format = "{:%H:%M:%S}";
          # format-alt = "{:%A, %B %d, %Y (%R)}";
          tooltip-format = "{:%Y-%m-%d}";

          # tooltip-format = ''
          #   <big>{:%Y %B}</big>
          #   <tt><small>{calendar}</small></tt>'';
          # calendar = {
          #   "mode"    = "year";
          #   "mode-mon-col"  = 3;
          #   "weeks-pos"     = "left";
          #   "on-scroll"     = 1;
          # };
          # "actions" =  {
          #     "on-click-right"= "mode";
          #     "on-scroll-up"= "shift_up";
          #     "on-scroll-down"= "shift_down";
          # };
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

        "pulseaudio/slider" = {
          rotate = 90;
          min = 0;
          max = 100;
          orientation = "vertical";
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
    min-width: 0;
    font-family: "JetBrainsMono Nerd Font", monospace;
    font-size: 13px;
    background: transparent;
    color: #d3c6aa;
  }

  tooltip {
    background: #2e383c;
    border: 1px solid rgba(100, 114, 125, 0.5);
  }
  tooltip label {
    color: #d3c6aa;
  }

  window#waybar {
    background: #272e33;
    color: #d3c6aa;
  }

  #workspaces button {
    padding: 4px 1px;
    margin: 2px 0;
    background: #2e383c;
    color: #859289;
    border: none;
  }
  #workspaces button.focused {
    background: #a7c080;
  }
  #workspaces button.focused label {
    color: #000000;
  }

  #tray,
  #network,
  #pulseaudio-slider,
  #battery,
  #battery#bat2,
  #clock {
    padding: 4px 1px 4px 0;
    margin: 2px 0;
    background: #2e383c;
    color: #d3c6aa;
  }

  #pulseaudio-slider slider {
    min-height: 0px;
    min-width: 0px;
    opacity: 0;
    background-image: none;
    border: none;
    box-shadow: none;
  }
  #pulseaudio-slider trough {
      min-height: 50px;
      min-width: 8px;
      border-radius: 0;
      background-color: #2e383c;
  }
  #pulseaudio-slider highlight {
      min-width: 10px;
      background-color: #a7c080;
  }

  '';
  };

}

