{ config, pkgs, lib, ... }:

{

  home.packages = with pkgs; [
    river
    fuzzel
    swaynotificationcenter
    # swaylock
    # swaybg
    wl-clipboard

    # foot
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
        "'*'" = "csd";

        # Make all views with app-id "bar" and any title use client-side decorations
        # "'bar'" = "csd";

        # Make all views with an app-id that starts with "float" and title "foo" start floating.
        # "'float*'"."-title"."'foo'" = "float";
      };

      set-repeat = "50 300";
      # set-cursor-warp = "on-output-change";
      # xcursor-theme = "someGreatTheme 12";

      spawn = [
        "firefox"
        "codium"
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
}
