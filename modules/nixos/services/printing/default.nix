{
  options,
  config,
  pkgs,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace};
let
  cfg = config.${namespace}.services.printing;
in
{
  options.${namespace}.services.printing = with types; {
    enable = mkBoolOpt false "Whether or not to configure printing + scanning support.";
  };

  config = mkIf cfg.enable {
    services = {
      # mDNS / DNS-SD: notwendig damit CUPS den WLAN-Drucker (IPP Everywhere) findet
      avahi = {
        enable = true;
        nssmdns4 = true;
        publish = {
          enable = true;
          addresses = true;
          userServices = true;
        };
      };

      # CUPS
      printing = {
        enable = true;

        # Für den DeskJet 3760 meist NICHT nötig:
        # driverless (IPP Everywhere) reicht. HPLIP ist optional (Status/Tools).
        drivers = [
          pkgs.hplip
        ];
      };
    };

    # Scanning (WLAN-Scanner über eSCL/AirScan)
    hardware.sane = {
      enable = true;
      extraBackends = [
        pkgs.sane-airscan
        pkgs.hplipWithPlugin
      ];
    };

    # Optional, aber oft praktisch für "lpadmin" & Co:
    programs.system-config-printer.enable = true;

    # Manche Setups brauchen das, damit mDNS zuverlässig funktioniert:
    services.resolved.enable = mkDefault true;
  };
}
