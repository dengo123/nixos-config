# modules/nixos/programs/steam/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.steam;
in {
  options.${namespace}.programs.steam = with types; {
    enable = mkBoolOpt false "Enable Steam.";

    remotePlayOpenFirewall =
      mkBoolOpt true
      "Open firewall ports for Steam Remote Play.";

    dedicatedServerOpenFirewall =
      mkBoolOpt false
      "Open firewall ports for Steam dedicated servers.";

    localNetworkGameTransfersOpenFirewall =
      mkBoolOpt true
      "Open firewall ports for Steam local network game transfers.";
  };

  config = mkIf cfg.enable {
    programs.steam = {
      enable = true;
      remotePlay.openFirewall = cfg.remotePlayOpenFirewall;
      dedicatedServer.openFirewall = cfg.dedicatedServerOpenFirewall;
      localNetworkGameTransfers.openFirewall = cfg.localNetworkGameTransfersOpenFirewall;
    };
  };
}
