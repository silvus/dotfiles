{ config, pkgs, ... }:

{
	home.packages = with pkgs; [
    hyprland
    kitty
    wofi
    xfce.thunar
    xfce.thunar-volman
    xfce.thunar-archive-plugin
    xfce.thunar-media-tags-plugin
    wev
    nwg-look
	];

  # programs. = {
  #   enable = true;
  # };


  wayland.windowManager.hyprland = {
    enable = true;
    # set the Hyprland and XDPH packages to null to use the ones from the NixOS module
    package = null;
    portalPackage = null;
    settings = {
      "$mod" = "SUPER";
      "$terminal" = "kitty";
      "$fileManager" = "thunar";
      "$menu" = "wofi --show drun";

      # Display
      "monitor " = ",preferred,auto,auto";

      # Autostart
      "exec-once" = "waybar";

      # Env
      # env = XCURSOR_SIZE,24
      # env = HYPRCURSOR_SIZE,24
      # env = HYPRCURSOR_THEME, Adwaita

      general =  {
        "gaps_in" = 5;
        "gaps_out" = 0;

        "border_size" = 2;

        # https://wiki.hyprland.org/Configuring/Variables/#variable-types for info about colors
        "col.active_border" = "rgba(33ccffee) rgba(00ff99ee) 45deg";
        "col.inactive_border" = "rgba(595959aa)";

        # Set to true enable resizing windows by clicking and dragging on borders and gaps
        "resize_on_border" = true;

        # Please see https://wiki.hyprland.org/Configuring/Tearing/ before you turn this on
        "allow_tearing" = false;

        "layout" = "dwindle";
      };

      # https://wiki.hyprland.org/Configuring/Variables/#decoration
      decoration = {
          "rounding" = 5;

          # Change transparency of focused and unfocused windows
          "active_opacity" = "1.0";
          "inactive_opacity" = "0.8";

          shadow = {
              "enabled" = true;
              "range" = 4;
              "render_power" = 3;
              "color" = "rgba(1a1a1aee)";
          };

          # https://wiki.hyprland.org/Configuring/Variables/#blur
          blur = {
              "enabled" = true;
              "size" = 3;
              "passes" = 1;

              "vibrancy" = "0.1696";
          };
      };

      # https://wiki.hyprland.org/Configuring/Variables/#animations
      animations = {
        "enabled" = true;

        # Default animations, see https://wiki.hyprland.org/Configuring/Animations/ for more
        "bezier" = [
          "easeOutQuint,0.23,1,0.32,1"
          "easeInOutCubic,0.65,0.05,0.36,1"
          "linear,0,0,1,1"
          "almostLinear,0.5,0.5,0.75,1.0"
          "quick,0.15,0,0.1,1"
        ];

        animation = [
          "global, 1, 10, default"
          "border, 1, 5.39, easeOutQuint"
          "windows, 1, 4.79, easeOutQuint"
          "windowsIn, 1, 4.1, easeOutQuint, popin 87%"
          "windowsOut, 1, 1.49, linear, popin 87%"
          "fadeIn, 1, 1.73, almostLinear"
          "fadeOut, 1, 1.46, almostLinear"
          "fade, 1, 3.03, quick"
          "layers, 1, 3.81, easeOutQuint"
          "layersIn, 1, 4, easeOutQuint, fade"
          "layersOut, 1, 1.5, linear, fade"
          "fadeLayersIn, 1, 1.79, almostLinear"
          "fadeLayersOut, 1, 1.39, almostLinear"
          "workspaces, 1, 1.94, almostLinear, fade"
          "workspacesIn, 1, 1.21, almostLinear, fade"
          "workspacesOut, 1, 1.94, almostLinear, fade"
        ];
      };

      # See https://wiki.hyprland.org/Configuring/Dwindle-Layout/ for more
      "dwindle" = {
          "pseudotile" = true; # Master switch for pseudotiling. Enabling is bound to mainMod + P in the keybinds section below
          "preserve_split" = true; # You probably want this
      };

      # See https://wiki.hyprland.org/Configuring/Master-Layout/ for more
      master = {
        "new_status" = "master";
      };

      # https://wiki.hyprland.org/Configuring/Variables/#misc
      misc = {
          "force_default_wallpaper" = -1; # Set to 0 or 1 to disable the anime mascot wallpapers
          "disable_hyprland_logo" = false; # If true disables the random hyprland logo / anime girl background. :(
      };

      input = {
        "kb_layout" = "us";
        "kb_variant" = "";
        "kb_model" = "";
        "kb_options" = "";
        "kb_rules" = "";

        "follow_mouse" = 0;

        "sensitivity" = 0; # -1.0 - 1.0, 0 means no modification.

        touchpad = {
            "natural_scroll" = false;
        };
      };

      # https://wiki.hyprland.org/Configuring/Variables/#gestures
      gestures = {
        "workspace_swipe" = false;
      };

      # Example per-device config
      # See https://wiki.hyprland.org/Configuring/Keywords/#per-device-input-configs for more
      # device = {
      #   "name" = "epic-mouse-v1";
      #   "sensitivity" = "-0.5";
      # };

      bind = [
        # "$mod, F, exec, firefox"
        # ", Print, exec, grimblast copy area"

        # Example binds, see https://wiki.hyprland.org/Configuring/Binds/ for more
        "$mod, RETURN, exec, $terminal"
        "$mod, W, killactive,"
        "$mod, Q, exit,"
        "$mod, F, exec, $fileManage"
        "$mod, SPACE, togglefloating,"
        "$mod, E, exec, $menu"
        "$mod, P, pseudo," # dwindle
        "$mod, J, togglesplit," # dwindle

        # Move focus with mainMod + arrow keys
        "$mod, left, movefocus, l"
        "$mod, right, movefocus, r"
        "$mod, up, movefocus, u"
        "$mod, down, movefocus, d"

        # Switch workspaces with mainMod + [0-9]
        "$mod, 1, workspace, 1"
        "$mod, 2, workspace, 2"
        "$mod, 3, workspace, 3"
        "$mod, 4, workspace, 4"
        "$mod, 5, workspace, 5"
        "$mod, 6, workspace, 6"
        "$mod, 7, workspace, 7"
        "$mod, 8, workspace, 8"
        "$mod, 9, workspace, 9"
        "$mod, 0, workspace, 10"

        # Move active window to a workspace with mainMod + SHIFT + [0-9]
        "$mod SHIFT, 1, movetoworkspace, 1"
        "$mod SHIFT, 2, movetoworkspace, 2"
        "$mod SHIFT, 3, movetoworkspace, 3"
        "$mod SHIFT, 4, movetoworkspace, 4"
        "$mod SHIFT, 5, movetoworkspace, 5"
        "$mod SHIFT, 6, movetoworkspace, 6"
        "$mod SHIFT, 7, movetoworkspace, 7"
        "$mod SHIFT, 8, movetoworkspace, 8"
        "$mod SHIFT, 9, movetoworkspace, 9"
        "$mod SHIFT, 0, movetoworkspace, 10"

        # Special workspace (scratchpad)
        ",grave, togglespecialworkspace, magic"
        "$mod SHIFT, grave, movetoworkspace, special:magic"

        # Scroll through existing workspaces with mainMod + scroll
        # bind = $mainMod, mouse_down, workspace, e+1
        # bind = $mainMod, mouse_up, workspace, e-1
      ];

      # Move/resize windows with mainMod + LMB/RMB and dragging
      bindm = [ 
        "$mod, mouse:272, movewindow"
        "$mod, mouse:273, resizewindow"
      ];

      # Laptop multimedia keys for volume and LCD brightness
      bindel = [
        ",XF86AudioRaiseVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+"
        ",XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"
        ",XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
        ",XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"
        ",XF86MonBrightnessUp, exec, brightnessctl s 10%+"
        ",XF86MonBrightnessDown, exec, brightnessctl s 10%-"
      ];
      
      # Requires playerctl
      bindl = [
        ", XF86AudioNext, exec, playerctl next"
        ", XF86AudioPause, exec, playerctl play-pause"
        ", XF86AudioPlay, exec, playerctl play-pause"
        ", XF86AudioPrev, exec, playerctl previous"
      ];

      # See https://wiki.hyprland.org/Configuring/Window-Rules/ for more
      # See https://wiki.hyprland.org/Configuring/Workspace-Rules/ for workspace rules
      windowrulev2 = [
        # Ignore maximize requests from apps. You'll probably like this.
        "suppressevent maximize, class:.*"
        # Fix some dragging issues with XWayland
        "nofocus,class:^$,title:^$,xwayland:1,floating:1,fullscreen:0,pinned:0"
      ];

    };
  };
  
}
