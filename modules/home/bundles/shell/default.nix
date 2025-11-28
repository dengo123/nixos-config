# modules/home/bundles/shell/default.nix
{
  config,
  pkgs,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace};
let
  cfg = config.${namespace}.bundles.shell;
in
{
  options.${namespace}.bundles.shell = with types; {
    enable = mkBoolOpt true "Enable shell bundle.";
    mode = mkOpt (types.enum [
      "full"
      "lite"
    ]) "full" "Select shell bundle mode: 'full' (alles) oder 'lite' (nur zsh + Zusatzpakete).";
  };

  config = mkIf cfg.enable (mkMerge [
    # Gemeinsame Zusatzpakete (in beiden Modi)
    {
      home.packages = with pkgs; [
        coreutils
        killall
        tldr
        wget
        ripgrep
      ];
    }

    # FULL: alles an
    (mkIf (cfg.mode == "full") {
      ${namespace}.programs = {
        atuin = enabled;
        btop = enabled;
        eza = enabled;
        fzf = enabled;
        starship = enabled;
        tmux = enabled;
        yazi = enabled;
        zoxide = enabled;
        zsh = enabled;
      };
    })

    # LITE: nur zsh (Rest aus)
    (mkIf (cfg.mode == "lite") {
      ${namespace}.programs = {
        zsh = enabled;
        btop = enabled;
      };
    })
  ]);
}
