{
  pkgs,
  config,
  lib,
  ...
}:

let
  mod = "Mod4";
  # mod = config.wayland.windowManager.sway.config.modifier;
in
{

  home.packages = with pkgs; [
    grim # screenshot
    slurp # screenshot region
    wl-clipboard
    wev # xev for wayland

    wezterm
    ghostty

    pavucontrol
    fuzzel
  ];

  # TODO rplace by fuzzel
  programs.wofi = {
    enable = true;
    settings = {
      allow_markup = true;
      width = 250;
    };
  };

  wayland.windowManager.sway = {
    enable = true;
    config = {
      modifier = mod;
      terminal = "wezterm";
      keybindings = lib.mkOptionDefault {
        # "${mod}+Return" = "exec ${cfg.config.terminal}";
        "${mod}+Return" = "exec wezterm";
        "${mod}+Shift+q" = "kill";
        "${mod}+e" = "exec --no-startup-id wofi --show drun,run";
        "${mod}+Shift+grave" = "move container to workspace number 0, workspace 0";
        "grave" = "workspace number 0";

        # Switch to workspace
        "${mod}+1" = "workspace number 1";
        "${mod}+2" = "workspace number 2";
        "${mod}+3" = "workspace number 3";
        "${mod}+4" = "workspace number 4";
        "${mod}+5" = "workspace number 5";
        "${mod}+6" = "workspace number 6";
        "${mod}+7" = "workspace number 7";
        "${mod}+8" = "workspace number 8";
        "${mod}+9" = "workspace number 9";
        "${mod}+0" = "workspace number 10";
        # Move focused container to workspace
        "${mod}+Shift+1" = "move container to workspace number 1, workspace 1";
        "${mod}+Shift+2" = "move container to workspace number 2, workspace 2";
        "${mod}+Shift+3" = "move container to workspace number 3, workspace 3";
        "${mod}+Shift+4" = "move container to workspace number 4, workspace 4";
        "${mod}+Shift+5" = "move container to workspace number 5, workspace 5";
        "${mod}+Shift+6" = "move container to workspace number 6, workspace 6";
        "${mod}+Shift+7" = "move container to workspace number 7, workspace 7";
        "${mod}+Shift+8" = "move container to workspace number 8, workspace 8";
        "${mod}+Shift+9" = "move container to workspace number 9, workspace 9";
        "${mod}+Shift+0" = "move container to workspace number 10, workspace 10";
        # Note: workspaces can have any name you want, not just numbers.
        # We just use 1-10 as the default.

        # Toggle control center
        "${mod}+n" = "exec swaync-client -t -sw";
      };
      # lib.attrsets.mergeAttrsList [
      # (lib.attrsets.mergeAttrsList (map (num: let
      #   ws = toString num;
      # in {
      #   "${mod}+${ws}" = "workspace ${ws}";
      #   "${mod}+Ctrl+${ws}" = "move container to workspace ${ws}";
      # }) [1 2 3 4 5 6 7 8 9 0]))

      # #
      # # Scratchpad:
      # #
      # # Sway has a "scratchpad", which is a bag of holding for windows.
      # # You can send windows there and get them back later.
      # # Move the currently focused window to the scratchpad
      # "${mod}+Shift+minus" = "move scratchpad";

      # # Show the next scratchpad window or hide the focused scratchpad window.
      # # If there are multiple scratchpad windows, this command cycles through them.
      # "minus" = "scratchpad show";

      # # Move your focus around
      # "${mod}+h" = "focus left";
      # "${mod}+j" = "focus down";
      # "${mod}+k" = "focus up";
      # "${mod}+l" = "focus right";
      # "${mod}+Left" = "focus left";
      # "${mod}+Down" = "focus down";
      # "${mod}+Up" = "focus up";
      # "${mod}+Right" = "focus right";
      # # Move the focused window with the same, but add Shift
      # "${mod}+Shift+h" = "move left";
      # "${mod}+Shift+j" = "move down";
      # "${mod}+Shift+k" = "move up";
      # "${mod}+Shift+l" = "move right";
      # "${mod}+Shift+Left move" = "left";
      # "${mod}+Shift+Down move" = "down";
      # "${mod}+Shift+Up move" = "up";
      # "${mod}+Shift+Right move" = "right";
      # # (lib.attrsets.concatMapAttrs (key: direction: {
      # #     "${mod}+${key}" = "focus ${direction}";
      # #     "${mod}+Ctrl+${key}" = "move ${direction}";
      # #   }) {
      # #     h = "left";
      # #     j = "down";
      # #     k = "up";
      # #     l = "right";`
      # #   })

      # "${mod}+Return" = "exec --no-startup-id ${pkgs.kitty}/bin/kitty";
      # "${mod}+e" = "exec --no-startup-id wofi --show drun,run";

      # "${mod}+x" = "kill";

      # "${mod}+a" = "focus parent";
      # "${mod}+c" = "layout toggle split";
      # "${mod}+f" = "fullscreen toggle";
      # "${mod}+g" = "split h";
      # "${mod}+s" = "layout stacking";
      # "${mod}+v" = "split v";
      # "${mod}+w" = "layout tabbed";

      # "${mod}+Shift+r" = "exec swaymsg reload";
      # "--release Print" = "exec --no-startup-id ${pkgs.sway-contrib.grimshot}/bin/grimshot copy area";
      # "${mod}+Ctrl+l" = "exec ${pkgs.swaylock-fancy}/bin/swaylock-fancy";
      # "${mod}+Ctrl+q" = "exit";
      # };

      # for_window = [
      #   {
      #     criteria = { class = "dropterm"; };
      #     command = "move to scratchpad, fullscreen enable";
      #   }
      # ];

      # Disable bar
      bars = [ ];

      focus.followMouse = false;
      startup = [
        { command = "ghostty --class=dropterm"; }
        # { command = "firefox"; }
        # { command = "codium"; }
        # https://github.com/Alexays/Waybar/issues/185#issuecomment-570340138
        { command = "GTK_THEME=Adapta waybar"; }
        # notification on workspace change
        # { command = "${config.home.homeDirectory}/.dotfiles/bin/sway-notify-workspace"; }
      ];
      defaultWorkspace = "workspace number 1";
      workspaceAutoBackAndForth = true;
    };

    # swaymsg -t get_tree
    extraConfig = ''
      for_window [app_id="dropterm"] move to workspace 0
      for_window [app_id="firefox"] move to workspace 1
      for_window [app_id="codium"] move to workspace 3

      gaps inner 5
      gaps outer 0

      # Set default border style for new tiled windows.
      default_border pixel 2
      # Set default border style for new floating windows. This only applies to windows that are spawned in floating mode, not windows that become floating afterwards.
      default_floating_border normal
    '';
    # for_window [class="dropterm"] floating enable;
    # for_window [class="dropterm"] move scratchpad; [instance="dropterm"] scratchpad show; fullscreen enable; move scratchpad

    systemd.enable = true;
    wrapperFeatures = {
      gtk = true;
    };
  };

  # programs.waybar = {
  #   enable = true;
  #   systemd.enable = true;
  # };

  home.file.".hm-graphical-session".text = pkgs.lib.concatStringsSep "\n" [
    "export MOZ_ENABLE_WAYLAND=1"
    "export NIXOS_OZONE_WL=1" # Electron
  ];

  # autorandr like
  services.kanshi = {
    enable = true;

    # swaymsg -t get_outputs
    settings = [
      {
        output = {
          criteria = "Red Hat, Inc. QEMU Monitor Unknown";
          status = "enable";
          mode = "1920x1080@60";
          scale = 1.0;
          alias = "VM";
        };
      }
      {
        profile = {
          name = "vm";
          outputs = [
            {
              criteria = "VM";
            }
          ];
        };
      }
    ];

    # home_office = {
    #   outputs = [
    #     {
    #       criteria = "DP-2";
    #       scale = 2.0;
    #       status = "enable";
    #       position = "0,0";
    #     }
    #     {
    #       criteria = "DP-1";
    #       scale = 2.0;
    #       status = "enable";
    #       position = "1920,0";
    #     }
    #     {
    #       criteria = "DP-3";
    #       scale = 2.0;
    #       status = "enable";
    #       position = "3840,0";
    #     }
    #   ];
    # };
  };

}
