{ pkgs, ... }:

{

  home.packages = with pkgs; [
    vscodium
  ];

  programs.vscode = {
    package = pkgs.vscodium;
    enable = true;

    profiles.default.keybindings = [
      {
        key = "ctrl+d";
        command = "editor.action.deleteLines";
        when = "textInputFocus && !editorReadonly";
      }
      {
        key = "ctrl+d";
        command = "-editor.action.addSelectionToNextFindMatch";
        when = "editorFocus";
      }
    ];

  };

}

