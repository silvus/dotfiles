{ pkgs, ... }:

{

  # Keyd keyboard remapping configuration
  # Converted from Kanata configuration + custom qwerty-fr XKB layout
  # Provides French accents on QWERTY layout with navigation layers
  # https://wiki.nixos.org/wiki/Keyd
  services.keyd = {
    enable = true;
    keyboards = {
      # The name is just the name of the configuration file, it does not really matter
      default = {
        ids = [ "*" ]; # what goes into the [id] section, here we select all keyboards
        settings = {
          main = {
            # Auto-shift behavior: tap for normal key, hold for shifted version
            "1" = "overload(shift, 1)";
            "2" = "overload(shift, 2)";
            "3" = "overload(shift, 3)";
            "4" = "overload(shift, 4)";
            "5" = "overload(shift, 5)";
            "6" = "overload(shift, 6)";
            "7" = "overload(shift, 7)";
            "8" = "overload(shift, 8)";
            "9" = "overload(shift, 9)";
            "0" = "overload(shift, 0)";
            "-" = "overload(shift, -)";
            "=" = "overload(shift, =)";

            "tab" = "overload(shift, tab)";
            "q" = "overload(shift, q)";
            "w" = "overload(shift, w)";
            "e" = "overload(shift, e)";
            "r" = "overload(shift, r)";
            "t" = "overload(shift, t)";
            "y" = "overload(shift, y)";
            "u" = "overload(shift, u)";
            "i" = "overload(shift, i)";
            "o" = "overload(shift, o)";
            "p" = "overload(shift, p)";
            "[" = "overload(shift, [)";
            "]" = "overload(shift, ])";
            "\\" = "overload(shift, \\)";

            "a" = "overload(shift, a)";
            "s" = "overload(shift, s)";
            "d" = "overload(shift, d)";
            "f" = "overload(shift, f)";
            "g" = "overload(shift, g)";
            "h" = "overload(shift, h)";
            "j" = "overload(shift, j)";
            "k" = "overload(shift, k)";
            "l" = "overload(shift, l)";
            ";" = "overload(shift, ;)";
            "'" = "overload(shift, ')";

            "z" = "overload(shift, z)";
            "x" = "overload(shift, x)";
            "c" = "overload(shift, c)";
            "v" = "overload(shift, v)";
            "b" = "overload(shift, b)";
            "n" = "overload(shift, n)";
            "m" = "overload(shift, m)";
            "," = "overload(shift, ,)";
            "." = "overload(shift, .)";
            "/" = "overload(shift, /)";

            # Special key mappings from Kanata
            "grave" = "overload(media, grave)";      # tap: grave, hold: media layer
            "space" = "overload(nav, space)";        # tap: space, hold: nav layer
            "capslock" = "overload(nav, esc)";       # tap: esc, hold: nav layer

            # Additional mappings for French accents (AltGr equivalent)
            "rightalt" = "layer(french)";            # AltGr for French accents
          };

          # Navigation layer (activated by holding space - from Kanata)
          nav = {
            "q" = "nop";
            "w" = "nop";
            "e" = "nop";
            "r" = "nop";
            "t" = "nop";
            "y" = "pageup";
            "u" = "home";
            "i" = "up";
            "o" = "end";
            "p" = "backspace";

            "a" = "nop";
            "s" = "nop";
            "d" = "nop";
            "f" = "nop";
            "g" = "nop";
            "h" = "pagedown";
            "j" = "left";
            "k" = "down";
            "l" = "right";
            ";" = "enter";
            "'" = "enter";
          };

          # Media layer (activated by holding grave - from Kanata)
          media = {
            "q" = "playpause";
            "w" = "previoussong";
            "e" = "nextsong";

            "z" = "volumedown";
            "x" = "mute";
            "c" = "volumeup";
          };

          # French accents layer (activated by holding capslock or AltGr - from XKB)
          french = {
            # Numbers row with accents and special characters
            "1" = "onesuperior";        # ¹
            "2" = "twosuperior";        # ²
            "3" = "ecircumflex";        # ê
            "4" = "EuroSign";           # €
            "6" = "ucircumflex";        # û
            "7" = "icircumflex";        # î
            "8" = "ocircumflex";        # ô

            # QWERTY row with French characters
            "q" = "acircumflex";        # â
            "w" = "egrave";             # è
            "e" = "eacute";             # é
            "u" = "ugrave";             # ù
            "i" = "igrave";             # ì
            "o" = "ograve";             # ò
            "p" = "oe";                 # œ

            # ASDF row with French characters
            "a" = "agrave";             # à
            "s" = "ae";                 # æ
            "d" = "ediaeresis";         # ë
            "h" = "ydiaeresis";         # ÿ
            "j" = "udiaeresis";         # ü
            "k" = "idiaeresis";         # ï
            "l" = "odiaeresis";         # ö

            # ZXCV row with French characters
            "z" = "adiaeresis";         # ä
            "c" = "ccedilla";           # ç
            "b" = "ssharp";             # ß
            "n" = "ntilde";             # ñ
          };
        };
      };
    };
  };
}
