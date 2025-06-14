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

        modules-center = [
          "cpu"
          "memory"
          "clock"
        ];

        modules-right = [
          "tray"
          "network"
          "pulseaudio"
          "battery"
          "battery#bat2"
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
         "sway/window" = {
          rotate = 90;
        };

        cpu = {
          rotate = 90;
          # format = "  {usage}%";
        };
        memory = {
          rotate = 90;
          # format = "  {}%";
          # interval = 5;
        };

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
          /* `otf-font-awesome` is required to be installed for icons */
          font-family: FontAwesome, Roboto, Helvetica, Arial, sans-serif;
          font-size: 13px;
          min-height: 0;
          min-width: 0;
      }

      window#waybar {
          background-color: rgba(43, 48, 59, 0.5);
          border-bottom: 3px solid rgba(100, 114, 125, 0.5);
          color: #ffffff;
          transition-property: background-color;
          transition-duration: .5s;
      }

      window#waybar.hidden {
          opacity: 0.2;
      }

      /*
      window#waybar.empty {
          background-color: transparent;
      }
      window#waybar.solo {
          background-color: #FFFFFF;
      }
      */

      window#waybar.termite {
          background-color: #3F3F3F;
      }

      window#waybar.chromium {
          background-color: #000000;
          border: none;
      }

      button {
          /* Use box-shadow instead of border so the text isn't offset */
          box-shadow: inset 0 -3px transparent;
          /* Avoid rounded borders under each button name */
          border: none;
          border-radius: 0;
      }

      /* https://github.com/Alexays/Waybar/wiki/FAQ#the-workspace-buttons-have-a-strange-hover-effect */
      button:hover {
          background: inherit;
          box-shadow: inset 0 -3px #ffffff;
      }

      /* you can set a style on hover for any module like this */
      #pulseaudio:hover {
          background-color: #a37800;
      }

      #workspaces button {
          padding: 0 5px;
          background-color: transparent;
          color: #ffffff;
      }

      #workspaces button:hover {
          background: rgba(0, 0, 0, 0.2);
      }

      #workspaces button.focused {
          background-color: #64727D;
          box-shadow: inset 0 -3px #ffffff;
      }

      #workspaces button.urgent {
          background-color: #eb4d4b;
      }

      #mode {
          background-color: #64727D;
          box-shadow: inset 0 -3px #ffffff;
      }

      #clock,
      #battery,
      #cpu,
      #memory,
      #disk,
      #temperature,
      #backlight,
      #network,
      #pulseaudio,
      #wireplumber,
      #custom-media,
      #tray,
      #mode,
      #idle_inhibitor,
      #scratchpad,
      #power-profiles-daemon,
      #mpd {
        padding: 0;
        margin: 0;
        color: #ffffff;
      }

      #window,
      #workspaces {
        padding: 0;
        margin: 0;
      }

      /* If workspaces is the leftmost module, omit left margin */
      .modules-left > widget:first-child > #workspaces {
          margin-left: 0;
      }

      /* If workspaces is the rightmost module, omit right margin */
      .modules-right > widget:last-child > #workspaces {
          margin-right: 0;
      }

      #clock {
          background-color: #64727D;
      }

      #battery {
          background-color: #ffffff;
          color: #000000;
      }

      #battery.charging, #battery.plugged {
          color: #ffffff;
          background-color: #26A65B;
      }

      @keyframes blink {
          to {
              background-color: #ffffff;
              color: #000000;
          }
      }

      /* Using steps() instead of linear as a timing function to limit cpu usage */
      #battery.critical:not(.charging) {
          background-color: #f53c3c;
          color: #ffffff;
          animation-name: blink;
          animation-duration: 0.5s;
          animation-timing-function: steps(12);
          animation-iteration-count: infinite;
          animation-direction: alternate;
      }

      #power-profiles-daemon {
          padding-right: 15px;
      }

      #power-profiles-daemon.performance {
          background-color: #f53c3c;
          color: #ffffff;
      }

      #power-profiles-daemon.balanced {
          background-color: #2980b9;
          color: #ffffff;
      }

      #power-profiles-daemon.power-saver {
          background-color: #2ecc71;
          color: #000000;
      }

      label:focus {
          background-color: #000000;
      }

      #cpu {
          background-color: #2ecc71;
          color: #000000;
      }

      #memory {
          background-color: #9b59b6;
      }

      #disk {
          background-color: #964B00;
      }

      #backlight {
          background-color: #90b1b1;
      }

      #network {
          background-color: #2980b9;
      }

      #network.disconnected {
          background-color: #f53c3c;
      }

      #pulseaudio {
          background-color: #f1c40f;
          color: #000000;
      }

      #pulseaudio.muted {
          background-color: #90b1b1;
          color: #2a5c45;
      }

      #wireplumber {
          background-color: #fff0f5;
          color: #000000;
      }

      #wireplumber.muted {
          background-color: #f53c3c;
      }

      #custom-media {
          background-color: #66cc99;
          color: #2a5c45;
          min-width: 100px;
      }

      #custom-media.custom-spotify {
          background-color: #66cc99;
      }

      #custom-media.custom-vlc {
          background-color: #ffa000;
      }

      #temperature {
          background-color: #f0932b;
      }

      #temperature.critical {
          background-color: #eb4d4b;
      }

      #tray {
          background-color: #2980b9;
      }

      #tray > .passive {
          -gtk-icon-effect: dim;
      }

      #tray > .needs-attention {
          -gtk-icon-effect: highlight;
          background-color: #eb4d4b;
      }

      #idle_inhibitor {
          background-color: #2d3436;
      }

      #idle_inhibitor.activated {
          background-color: #ecf0f1;
          color: #2d3436;
      }

      #mpd {
          background-color: #66cc99;
          color: #2a5c45;
      }

      #mpd.disconnected {
          background-color: #f53c3c;
      }

      #mpd.stopped {
          background-color: #90b1b1;
      }

      #mpd.paused {
          background-color: #51a37a;
      }

      #language {
          background: #00b093;
          color: #740864;
          padding: 0 5px;
          margin: 0 5px;
          min-width: 16px;
      }

      #keyboard-state {
          background: #97e1ad;
          color: #000000;
          padding: 0 0px;
          margin: 0 5px;
          min-width: 16px;
      }

      #keyboard-state > label {
          padding: 0 5px;
      }

      #keyboard-state > label.locked {
          background: rgba(0, 0, 0, 0.2);
      }

      #scratchpad {
          background: rgba(0, 0, 0, 0.2);
      }

      #scratchpad.empty {
        background-color: transparent;
      }

      #privacy {
          padding: 0;
      }

      #privacy-item {
          padding: 0 5px;
          color: white;
      }

      #privacy-item.screenshare {
          background-color: #cf5700;
      }

      #privacy-item.audio-in {
          background-color: #1ca000;
      }

      #privacy-item.audio-out {
          background-color: #0069d4;
      }
    '';
  };

}

