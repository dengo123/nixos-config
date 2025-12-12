# modules/home/programs/starship/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace};
let
  cfg = config.${namespace}.programs.starship;

  # kleine Helper: '#'-Prefix sicher entfernen + '#' wieder davorsetzen
  hex =
    c:
    let
      s = lib.removePrefix "#" c;
    in
    "#${s}";

  # Palette laden (ohne Stylix!)
  # Passe den Default-Pfad an deine Struktur an:
  defaultPalette = import ../../misc/stylix/base16/luna-blue.nix;

  pal = if cfg.palette != null then cfg.palette else defaultPalette;

  color = name: hex (builtins.getAttr name pal);
in
{
  options.${namespace}.programs.starship = with types; {
    enable = mkBoolOpt false "Enable starship";
    # Optional: du kannst auch eine Palette direkt überschreiben (als Attrset)
    palette =
      mkOpt (types.nullOr types.attrs) null
        "Optional Base16 palette attrset to use instead of the default.";
  };

  config = mkIf cfg.enable {
    programs.starship = {
      enable = true;
      settings = {
        command_timeout = 5000;

        format = lib.concatStrings [
          "[](${color "base0B"})"
          "$os$username"
          "[](bg:${color "base0E"} fg:${color "base0B"})"
          "$directory"
          "[](fg:${color "base0E"} bg:${color "base0D"})"
          "$git_branch$git_status"
          "[](fg:${color "base0D"} bg:${color "base0D"})"
          "$c$rust$golang$nodejs$php$java$kotlin$haskell$python"
          "[](fg:${color "base0D"} bg:${color "base0C"})"
          "$docker_context"
          "[](fg:${color "base0C"} bg:${color "base0C"})"
          "$time"
          "[ ](fg:${color "base0C"})"
          "$line_break$character"
        ];

        os = {
          disabled = false;
          style = "bg:${color "base0B"} fg:${color "base08"}";
          symbols = {
            NixOS = "󱄅";
          };
        };

        username = {
          show_always = true;
          style_user = "bg:${color "base0B"} fg:${color "base01"}";
          style_root = "bg:${color "base0B"} fg:${color "base01"}";
          format = "[ $user ]($style)";
        };

        directory = {
          style = "bg:${color "base0E"} fg:${color "base01"}";
          format = "[ $path ]($style)";
          truncation_length = 3;
          truncation_symbol = "…/";
          substitutions = {
            "Documents" = "󰈙 ";
            "Downloads" = " ";
            "Music" = " ";
            "Pictures" = " ";
            "Developer" = "󰲋 ";
          };
        };

        git_branch = {
          symbol = "";
          style = "bg:${color "base0D"}";
          format = "[[ $symbol $branch ](fg:${color "base02"} bg:${color "base0D"})]($style)";
        };
        git_status = {
          style = "bg:${color "base0D"}";
          format = "[[($all_status$ahead_behind )](fg:${color "base02"} bg:${color "base0D"})]($style)";
        };

        nodejs = {
          symbol = "";
          style = "bg:${color "base0D"}";
          format = "[[ $symbol( $version) ](fg:${color "base02"} bg:${color "base0D"})]($style)";
        };
        c = {
          symbol = " ";
          style = "bg:${color "base0D"}";
          format = " $symbol ($version) ]($style)";
        };
        rust = {
          symbol = "";
          style = "bg:${color "base0D"}";
          format = "[ $symbol ($version) ]($style)";
        };
        golang = {
          symbol = " ";
          style = "bg:${color "base0D"}";
          format = "[ $symbol ($version) ]($style)";
        };
        php = {
          style = "bg:${color "base0D"}";
          format = "[ $symbol ($version) ]($style)";
        };
        java = {
          symbol = " ";
          style = "bg:${color "base0D"}";
          format = "[ $symbol ($version) ]($style)";
        };
        kotlin = {
          style = "bg:${color "base0D"}";
          format = "[ $symbol ($version) ]($style)";
        };
        haskell = {
          symbol = " ";
          style = "bg:${color "base0D"}";
          format = "[ $symbol ($version) ]($style)";
        };
        python = {
          style = "bg:${color "base0D"}";
          format = "[(\\($virtualenv\\) )]($style)";
        };
        docker_context = {
          symbol = " ";
          style = "bg:${color "base0D"}";
          format = "[ $symbol $context ]($style) $path";
        };

        time = {
          disabled = false;
          time_format = "%R";
          style = "bg:${color "base09"}";
          format = "[[  $time ](fg:${color "base02"} bg:${color "base0C"})]($style)";
        };

        line_break.disabled = false;
        character = {
          disabled = false;
          success_symbol = "[](bold fg:${color "base0B"})";
          error_symbol = "[](bold fg:${color "base08"})";
          vimcmd_symbol = "[](bold fg:${color "base05"})";
          vimcmd_replace_one_symbol = "[](bold fg:${color "base0E"})";
          vimcmd_replace_symbol = "[](bold fg:${color "base0E"})";
          vimcmd_visual_symbol = "[](bold fg:${color "base07"})";
        };
      };
    };
  };
}
