{
  lib,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.system.keyboard;
in {
  options.${namespace}.system.keyboard = {
    enable = mkBoolOpt true "Enable keyboard config";
    layout = mkOpt types.str "us" "Keyboard layout";
    variant = mkOpt types.str "intl" "Keyboard variant";
    options = mkOpt (types.listOf types.str) [] "XKB options as list of strings";
  };

  config = mkIf cfg.enable {
    services.xserver.xkb = {
      layout = cfg.layout;
      variant = cfg.variant;
      options = lib.concatStringsSep "," cfg.options;
    };
  };
}
