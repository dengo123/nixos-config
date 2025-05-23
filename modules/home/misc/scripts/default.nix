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
      auto-monitors = enabled;
      brightness = enabled;
      caffeine = enabled;
      hyprfocus = enabled;
      hyprpanel = enabled;
      night_shift = enabled;
      notification = enabled;
      screenshot = enabled;
      sounds = enabled;
      system = enabled;
    };
  };
}
