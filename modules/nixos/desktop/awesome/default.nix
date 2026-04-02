# modules/nixos/desktop/awesome/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.desktop.awesome;

  awesomePkg =
    if cfg.package == "patched"
    then pkgs.awesome-patched
    else pkgs.awesome;
in {
  options.${namespace}.desktop.awesome = with types; {
    enable = mkBoolOpt false "Enable Awesome WM on X11.";

    package = mkOpt (enum [
      "stable"
      "patched"
    ]) "stable" "Which Awesome package to use.";
  };

  config = mkIf cfg.enable {
    services.xserver.enable = true;
    services.libinput.enable = true;

    services.xserver.windowManager.awesome = {
      enable = true;
      package = awesomePkg;
    };

    services.displayManager.defaultSession = "none+awesome";
  };
}
