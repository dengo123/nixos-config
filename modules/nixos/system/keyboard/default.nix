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
  options.${namespace}.system.keyboard = mkOpt types.attrs {
    layout = "us";
    variant = "intl";
    options = [];
  } "Keyboard layout and variant";

  config = {
    services.xserver = {
      layout = cfg.layout;
      xkbVariant = cfg.variant;
      xkbOptions = cfg.options;
    };
  };
}
