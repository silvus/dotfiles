{ pkgs, ... }:

let

  # Share php config between cli and fpm
  phpCustom = pkgs.php85.withExtensions ({ enabled, all }: enabled ++ [
    all.mbstring
    all.intl
    all.curl
    all.bcmath
    all.gd
    # all.xml # already in default
    all.sqlite3
    all.openssl
    all.dom
    all.bz2
    all.zip
  ]);

in {
  environment.systemPackages = with pkgs; [

    # PHP for cli
    phpCustom
    # php LSP
    # intelephense
  ];

}
