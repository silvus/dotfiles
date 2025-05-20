{ config, pkgs, lib, ... }:

{

  home.packages = with pkgs; [
    river
    fuzzel
    swaynotificationcenter
    # swaylock
    # swaybg
    wl-clipboard

    waybar
    pavucontrol
  ];

  wayland.windowManager.river = {
    enable = true;
    settings = {

      # keyboard-layout gb

      border-width = 2;
      # background-color = "0x002b36";
      # border-color-focused = "0x93a1a1";
      # border-color-unfocused = "0x586e75";
      background-color = "0x1E2326";        # Everforest background dark
      border-color-focused = "0x83C092";    # Everforest light yellow (contrast)
      border-color-unfocused = "0x4F5B58";  # Everforest gray tone (muted)

      declare-mode = [
        "locked"
        "normal"
        # Declare a passthrough mode. This mode has only a single mapping to return to
        # normal mode. This makes it useful for testing a nested wayland compositor
        # "passthrough"
      ];

      map.normal = {
        # Super+Shift+Return to start an instance of foot (https://codeberg.org/dnkl/foot)
        "Super+Shift Return" = "spawn wezterm";

        # Super+Q to close the focused view
        "Super Q" = "close";

        # Super+Shift+E to exit river
        "Super+Shift Q" = "exit";

        # Super+J and Super+K to focus the next/previous view in the layout stack
        "Super L" = "focus-view next";
        "Super J" = "focus-view previous";

        # Super+Shift+J and Super+Shift+K to swap the focused view with the next/previous
        # view in the layout stack
        "Super+Shift L" = "swap next";
        "Super+Shift J" = "swap previous";

        # Super+Period and Super+Comma to focus the next/previous output
        "Super Period" = "focus-output next";
        "Super Comma" = "focus-output previous";

        # Super+Shift+{Period,Comma} to send the focused view to the next/previous output
        "Super+Shift Period" = "send-to-output next";
        "Super+Shift Comma" = "send-to-output previous";

        # Super+Return to bump the focused view to the top of the layout stack
        "Super Return" = "zoom";

        # Super+H and Super+L to decrease/increase the main ratio of rivertile(1)
        "Super I" = "send-layout-cmd rivertile 'main-ratio -0.05'";
        "Super K" = "send-layout-cmd rivertile 'main-ratio +0.05'";

        # Super+Shift+H and Super+Shift+L to increment/decrement the main count of rivertile(1)
        "Super+Shift I" = "send-layout-cmd rivertile 'main-count +1'";
        "Super+Shift K" = "send-layout-cmd rivertile 'main-count -1'";

        # Super+Alt+{H,J,K,L} to move views
        "Super+Alt J" = "move left 100";
        "Super+Alt K" = "move down 100";
        "Super+Alt I" = "move up 100";
        "Super+Alt L" = "move right 100";

        # Super+Alt+Control+{H,J,K,L} to snap views to screen edges
        "Super+Alt+Control J" = "snap left";
        "Super+Alt+Control K" = "snap down";
        "Super+Alt+Control I" = "snap up";
        "Super+Alt+Control L" = "snap right";

        # Super+Alt+Shift+{H,J,K,L} to resize views
        "Super+Alt+Shift J" = "resize horizontal -100";
        "Super+Alt+Shift K" = "resize vertical 100";
        "Super+Alt+Shift I" = "resize vertical -100";
        "Super+Alt+Shift L" = "resize horizontal 100";

        # Super+[1-9] to focus tag [0-8]
        "Super 1" = "set-focused-tags 0";
        "Super 2" = "set-focused-tags 1";
        "Super 3" = "set-focused-tags 2";
        "Super 4" = "set-focused-tags 3";
        "Super 5" = "set-focused-tags 4";
        "Super 6" = "set-focused-tags 5";
        "Super 7" = "set-focused-tags 6";
        "Super 8" = "set-focused-tags 7";
        "Super 9" = "set-focused-tags 8";

        # Super+Shift+[1-9] to tag focused view with tag [0-8]
        "Super+Shift 1" = "set-view-tags 0";
        "Super+Shift 2" = "set-view-tags 1";
        "Super+Shift 3" = "set-view-tags 2";
        "Super+Shift 4" = "set-view-tags 3";
        "Super+Shift 5" = "set-view-tags 4";
        "Super+Shift 6" = "set-view-tags 5";
        "Super+Shift 7" = "set-view-tags 6";
        "Super+Shift 8" = "set-view-tags 7";
        "Super+Shift 9" = "set-view-tags 8";

        # Super+Control+[1-9] to toggle focus of tag [0-8]
        "Super+Control 1" = "toggle-focused-tags 0";
        "Super+Control 2" = "toggle-focused-tags 1";
        "Super+Control 3" = "toggle-focused-tags 2";
        "Super+Control 4" = "toggle-focused-tags 3";
        "Super+Control 5" = "toggle-focused-tags 4";
        "Super+Control 6" = "toggle-focused-tags 5";
        "Super+Control 7" = "toggle-focused-tags 6";
        "Super+Control 8" = "toggle-focused-tags 7";
        "Super+Control 9" = "toggle-focused-tags 8";

        # Super+Shift+Control+[1-9] to toggle tag [0-8] of focused view
        "Super+Shift+Control 1" = "toggle-view-tags 0";
        "Super+Shift+Control 2" = "toggle-view-tags 1";
        "Super+Shift+Control 3" = "toggle-view-tags 2";
        "Super+Shift+Control 4" = "toggle-view-tags 3";
        "Super+Shift+Control 5" = "toggle-view-tags 4";
        "Super+Shift+Control 6" = "toggle-view-tags 5";
        "Super+Shift+Control 7" = "toggle-view-tags 6";
        "Super+Shift+Control 8" = "toggle-view-tags 7";
        "Super+Shift+Control 9" = "toggle-view-tags 8";

        # Super+0 to focus all tags
        # Super+Shift+0 to tag focused view with all tags
        # all_tags=$(((1 << 32) - 1))
        # "Super 0" = "tset-focused-tags $all_tags";
        # "Super+Shift 0" = "tset-view-tags $all_tags";

        # Super+Space to toggle float
        "Super Space" = "toggle-float";

        # Super+F to toggle fullscreen
        "Super F" = "toggle-fullscreen";

        # Super+{Up,Right,Down,Left} to change layout orientation
        "Super Up" = "send-layout-cmd rivertile 'main-location top'";
        "Super Right" = "send-layout-cmd rivertile 'main-location right'";
        "Super Down" = "send-layout-cmd rivertile 'main-location bottom'";
        "Super Left" = "send-layout-cmd rivertile 'main-location left'";

        # Control pulse audio volume with pamixer (https://github.com/cdemoulins/pamixer)
        "None XF86AudioRaiseVolume" = "spawn 'pamixer -i 5'";
        "None XF86AudioLowerVolume" = "spawn 'pamixer -d 5'";
        "None XF86AudioMute" = "spawn 'pamixer --toggle-mute'";

        # Control MPRIS aware media players with playerctl (https://github.com/altdesktop/playerctl)
        "None XF86AudioMedia" = "spawn 'playerctl play-pause'";
        "None XF86AudioPlay" = "spawn 'playerctl play-pause'";
        "None XF86AudioPrev" = "spawn 'playerctl previous'";
        "None XF86AudioNext" = "spawn 'playerctl next'";

        # Control screen backlight brightness with brightnessctl (https://github.com/Hummer12007/brightnessctl)
        "None XF86MonBrightnessUp" = "spawn 'brightnessctl set +5%'";
        "None XF86MonBrightnessDown" = "spawn 'brightnessctl set 5%-'";

        # Super+F11 to enter passthrough mode
        # "Super F11 enter-mode passthrough"

        # Super+F11 to return to normal mode
        # riverctl map passthrough Super F11 enter-mode normal
      };

      rule-add."-app-id" = {
        # Use client-side decorations
        # "'*'" = "csd";

        # Make all views with app-id "bar" and any title use client-side decorations
        # "'bar'" = "csd";

        # Make all views with an app-id that starts with "float" and title "foo" start floating.
        # "'float*'"."-title"."'foo'" = "float";
      };

      set-repeat = "50 300";
      # set-cursor-warp = "on-output-change";
      # xcursor-theme = "someGreatTheme 12";

      spawn = [
        "waybar"
        "wezterm"
        # "firefox"
        # "codium"
        # "'foot -a terminal'"
      ];

      output-layout = "rivertile";
    };

    extraConfig = ''
      rivertile -view-padding 2 -outer-padding 4 &
    '';

    # systemd.enable = true;
    # wrapperFeatures = {gtk = true;};
  };

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
        # height = 20;
        width = 22;
        # margin = "6";
        # "spacing" = 4; # Gaps between modules
        position = "right";
        modules-left = [
          "sway/workspaces"
          # "river/mode"
          # "river/layout"
        ];

        modules-center = [
          # "river/window"
        ];

        modules-right = [
          # "cpu"
          # "memory "
          "network"
          "pulseaudio/slider"
          "pulseaudio"
          # # "battery"
          # # "battery#bat2"
          "tray"
          "clock"
        ];

   

        clock = {
          # rotate = 90;
          orientation = "vertical";
          interval = 1;
          format = "{:%H\n%M\n%S}";
          # format-alt = "{:%Y-%m-%d %H:%M:%S %z}";
          # on-click-left = "mode";
          tooltip-format = ''
            <big>{:%Y %B}</big>
            <tt><small>{calendar}</small></tt>'';
        };

        tray = {
          spacing = 10;
        };

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
          min = 0;
          max = 150;
          orientation = "vertical";
          on-click = "pavucontrol";
        };

        pulseaudio = {
          format = "{icon}";
          format-bluetooth = "{icon}";
          format-bluetooth-muted = "󰝟 {icon}";
          format-muted = "󰝟";
          format-source = "";
          format-source-muted = "";
          on-click = "pavucontrol";
          format-icons = {
            headphone = "";
            hands-free = "";
            headset = "";
            phone = "";
            portable = "";
            car = "";
            default = ["" "" ""];
          };
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
            format = "{icon}";
            # "rotate": 0,
            # "format-charging" = "<span color='#a6d189'>󱐋</span>";
            # "format-plugged" = "󰂄";
            # "format-icons" = [
            #   "󰝦"
            #   "󰪞"
            #   "󰪟"
            #   "󰪠"
            #   "󰪡"
            #   "󰪢"
            #   "󰪣"
            #   "󰪤"
            #   "󰪥"
            # ];
            format-icons = ["<span color='#f38ba8'>󰂎</span>" "<span color='#fab387'>󰁺</span>" "<span color='#f9e2af'>󰁻</span>""<span color='#f9e2af'>󰁼</span>" "󰁽" "󰁾" "󰁿" "󰂀" "󰂁" "󰂂" "󰁹"];
            format-charging = ["󰢟" "󰢜" "󰂆" "󰂇" "󰂈" "󰢝" "󰂉" "󰢞" "󰂊" "󰂋" "󰂅"];
            tooltip = true;
            tooltip-format =  "{capacity}%";
        };

        "battery#bat2" = {
            format = "{icon}";
            # "rotate": 0;
            format-charging = "<span color='#a6d189'>󱐋</span>";
            format-plugged = "󰂄";
            format-icons = [
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
          # rotate = 90;
          interval = 3;
          format-wifi = "";
          format-ethernet = "󰈁";
          format-linked = "󰈁";
          format-disconnected = "⚠";
          # format-wifi = " {essid}";
          # format-ethernet = "󰈁 Connected";
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
  padding: 0;
  margin: 0;
  border-radius: 0;
  box-shadow: none;
  border: none;
}

#waybar {
  color: #a3be8c;
  border: 0 solid #a3be8c;
  background-color: #3B4252;
}

#tray,
#battery,
#network,
#clock,
#pulseaudio,
#pulseaudio-slider {
  background-color: #4c566a;
  font-size: 24px;
  padding: 4px 0;
  margin: 4px 2px;
  font-weight: bold;
}

#clock {
  font-size: 10px;
}

#pulseaudio {
  font-size: 12px;
  margin-top: 0;
  padding-right: 6px;
}
#pulseaudio-slider {
  margin-bottom: 0;
}
#pulseaudio-slider slider {
  min-width: 12px;
  min-height: 60px;
  background: none transparent;
  padding: 0;
  margin: 2px 2px 0px 2px;
}
#pulseaudio-slider trough {
  min-height: 0;
  padding: 0;
  margin: 0;
  background: none transparent;
  opacity: 0;
}
#pulseaudio-slider highlight {
  min-width: 14px;
  min-height: 0px;
  padding: 0;
  margin: 0;
}

#tags {
  background-color: #4c566a;
  padding: 0;
  margin: 0;

  min-height: 60px;
}
#tag button {
  padding: 0;
  margin: 0;
  color: #d8dee9;

  min-height: 60px;
}

#tags button.urgent{
  color: #bf616a;
}
#tags button.occupied{
  color: #89b4fa;
}
#tags button.focused {
  color: #a3be8c;
}
#tags button,
#tags button:hover {
  background: transparent;    
  border: none;       
  box-shadow: none;   
  background: transparent; 
  text-shadow: none;  

  min-height: 60px;
}

tooltip {
  background-color: #3B4252;
  color: #a3be8c;
  border: 1px solid #a3be8c;
  border-radius: 2px;
}

tooltip, tooltip * {
  color: #a3be8c;
  font-weight: bold;
}

'';
  };
}
