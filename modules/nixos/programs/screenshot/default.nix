# modules/programs/screenshot/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.screenshot;
in {
  options.${namespace}.programs.screenshot = {
    enable = mkBoolOpt false "Enable X11 screenshots via maim + slop.";

    editor = {
      enable = mkBoolOpt false "Enable a GUI screenshot editor.";
      # bewusst schmal gehalten, GTK3-first. 'flameshot' & 'ksnip' als Alternativen
      type = mkOpt (types.enum [
        "shutter"
        "flameshot"
        "ksnip"
      ]) "shutter" "Choose a GUI editor (GTK3 'shutter', or alternatives 'flameshot'/'ksnip').";
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages =
      (with pkgs; [
        maim
        slop
      ])
      ++ (
        with pkgs;
          optional (cfg.editor.enable && cfg.editor.type == "shutter") shutter
          ++ optional (cfg.editor.enable && cfg.editor.type == "flameshot") flameshot
          ++ optional (cfg.editor.enable && cfg.editor.type == "ksnip") ksnip
      );

    # keine Pfadvorgaben hier – Save-Dir & Logik bitte in Awesome-Keybinds regeln.
  };
}
