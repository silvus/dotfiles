{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # Office
    thunderbird            # Mail and calendar
    signal-desktop         # Communication
    libreoffice            # Office suite
    simple-scan            # Document scanner
    gimp                   # Painting
    drawio                 # Diagrams
    alarm-clock-applet     # Alarm and timer
    zathura                # PDF
    cheese                 # Webcam testing
    mediaelch              # TVshow manager
    translate-shell        # Command-line translator

    typst
  ];
}
