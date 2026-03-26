# modules/home/misc/scripts/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.misc.scripts;
in {
  options.${namespace}.misc.scripts = with types; {
    enable = mkBoolOpt false "Enable misc.scripts";
  };

  config = mkIf cfg.enable {
    nixforge.misc.scripts = {
      autorandr-toggle = enabled;
      apply-gtk-theme = enabled;
      apply-starship-theme = enabled;
      # brightness = enabled;
    };
  };
}
