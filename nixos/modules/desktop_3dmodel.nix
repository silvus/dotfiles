{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # 3D
    # unstable.bambu-studio # 2026-01-02 Cannot login
    unstable.orca-slicer
    freecad
    # blender

  ];
}
