{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.hardware.networking;
in {
  options.${namespace}.hardware.networking = with types; {
    enable = mkBoolOpt false "Enable NetworkManager.";
    trayApplet = mkBoolOpt true "Install nm-applet binaries.";
  };

  config = mkIf cfg.enable {
    networking.networkmanager.enable = true;
    environment.systemPackages = mkIf cfg.trayApplet [pkgs.networkmanagerapplet];
  };
}
