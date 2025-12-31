{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace};
let
  cfg = config.${namespace}.hardware.rgb;
in
{
  options.${namespace}.hardware.rgb = with types; {
    enable = mkBoolOpt false "Enable OpenRGB.";
    motherboard = mkOpt (nullOr str) "amd" "null or one of 'amd' or 'intel'";
    port = mkIntOpt 6742 "Set server port of openrgb.";
    profile = mkOpt (nullOr str) null "The profile to load from 'var/lib/OpenRGB' at startup.";
  };

  config = mkIf cfg.enable {
    services.hardware.openrgb = {
      enable = true;
      package = pkgs.openrgb-with-all-plugins;
      motherboard = cfg.motherboard;
      server.port = cfg.port;
      startupProfile = cfg.profile;
    };
  };
}
