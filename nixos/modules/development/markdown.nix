{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # Markdown
    marksman
    markdown-oxide
    # harper
  ];
}
