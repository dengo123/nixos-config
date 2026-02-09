# modules/home/programs/autorandr/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace};

let
  cfg = config.${namespace}.programs.autorandr;
in
{
  options.${namespace}.programs.autorandr = {
    enable = mkEnableOption "Enable autorandr (X11 only)";
  };

  config = mkIf cfg.enable {

    programs.autorandr = {
      enable = true;
    };
  };
}
