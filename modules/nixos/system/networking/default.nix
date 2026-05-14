# modules/nixos/system/networking/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.system.networking;
in {
  options.${namespace}.system.networking = with types; {
    enable = mkBoolOpt true "Enable base networking configuration.";

    networkmanager = {
      enable = mkBoolOpt false "Enable NetworkManager.";
      waitOnline = mkBoolOpt false "Whether NetworkManager should wait for full network connectivity during boot.";
    };

    firewall = {
      enable = mkBoolOpt true "Enable the firewall.";

      allowedTCPPorts = mkOpt (listOf port) [] "Allowed TCP ports.";
      allowedUDPPorts = mkOpt (listOf port) [] "Allowed UDP ports.";

      trustedInterfaces = mkOpt (listOf str) [] "Interfaces trusted by the firewall.";
      allowPing = mkBoolOpt true "Whether to allow ICMP echo requests.";
      logReversePathDrops = mkBoolOpt false "Whether to log reverse path filter drops.";
    };

    hardening = {
      enable = mkBoolOpt true "Enable basic network hardening sysctls.";
      rpFilter = mkBoolOpt true "Enable reverse path filtering.";
      tcpSyncookies = mkBoolOpt true "Enable TCP syncookies.";
      acceptRedirects = mkBoolOpt false "Accept ICMP redirects.";
      sendRedirects = mkBoolOpt false "Send ICMP redirects.";
      acceptSourceRoute = mkBoolOpt false "Accept source-routed packets.";
    };
  };

  config = mkIf cfg.enable {
    networking = {
      networkmanager = mkIf cfg.networkmanager.enable {
        enable = true;
      };

      firewall = {
        enable = cfg.firewall.enable;
        allowedTCPPorts = cfg.firewall.allowedTCPPorts;
        allowedUDPPorts = cfg.firewall.allowedUDPPorts;
        trustedInterfaces = cfg.firewall.trustedInterfaces;
        allowPing = cfg.firewall.allowPing;
        logReversePathDrops = cfg.firewall.logReversePathDrops;
      };
    };

    systemd.services.NetworkManager-wait-online.enable = mkIf cfg.networkmanager.enable cfg.networkmanager.waitOnline;

    boot.kernel.sysctl = mkIf cfg.hardening.enable (
      {}
      // optionalAttrs cfg.hardening.rpFilter {
        "net.ipv4.conf.all.rp_filter" = 1;
        "net.ipv4.conf.default.rp_filter" = 1;
      }
      // optionalAttrs cfg.hardening.tcpSyncookies {
        "net.ipv4.tcp_syncookies" = 1;
      }
      // {
        "net.ipv4.conf.all.accept_redirects" =
          if cfg.hardening.acceptRedirects
          then 1
          else 0;
        "net.ipv4.conf.default.accept_redirects" =
          if cfg.hardening.acceptRedirects
          then 1
          else 0;

        "net.ipv4.conf.all.send_redirects" =
          if cfg.hardening.sendRedirects
          then 1
          else 0;
        "net.ipv4.conf.default.send_redirects" =
          if cfg.hardening.sendRedirects
          then 1
          else 0;

        "net.ipv4.conf.all.accept_source_route" =
          if cfg.hardening.acceptSourceRoute
          then 1
          else 0;
        "net.ipv4.conf.default.accept_source_route" =
          if cfg.hardening.acceptSourceRoute
          then 1
          else 0;
      }
    );
  };
}
