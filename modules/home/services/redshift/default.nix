# modules/home/programs/redshift/default.nix
{
  lib,
  pkgs,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.redshift;
in {
  options.${namespace}.services.redshift = with types; {
    enable    = mkBoolOpt false "Enable redshift via services.redshift.";
    package   = mkOpt types.package pkgs.redshift "Redshift-Paket.";
    provider  = mkOpt (types.enum [ "manual" "geoclue2" ]) "manual" "Standortquelle.";
    latitude  = mkOpt (types.nullOr types.number) null "Breitengrad (bei provider=manual).";
    longitude = mkOpt (types.nullOr types.number) null "Längengrad (bei provider=manual).";

    temperature = mkOpt (types.nullOr (types.submodule {
      options.day   = mkOpt types.int 5500 "Tag (K)";
      options.night = mkOpt types.int 3800 "Nacht (K)";
    })) { day = 5500; night = 3800; } "Farbtemperaturen in Kelvin.";

    brightness = mkOpt (types.nullOr (types.submodule {
      options.day   = mkOpt types.number 1.0 "Tag (0–1)";
      options.night = mkOpt types.number 0.9 "Nacht (0–1)";
    })) { day = 1.0; night = 0.9; } "Helligkeit (Multiplikator).";

    tray = mkBoolOpt false "Tray-Icon aktivieren (falls vom Desktop unterstützt).";
    extraOptions = mkOpt (types.attrsOf types.anything) { } "Weitere HM-Optionen direkt an services.redshift durchreichen.";
  };

  config = mkIf cfg.enable {
    # Assertions nur wenn manual
    assertions = [
      {
        assertion = cfg.provider != "manual" || (cfg.latitude != null && cfg.longitude != null);
        message = "${namespace}.programs.redshift: provider=manual benötigt latitude und longitude.";
      }
    ];

    # Einfaches Mapping auf Home-Manager:
    services.redshift = {
      enable   = true;
      package  = cfg.package;
      provider = cfg.provider;
      latitude = cfg.latitude;
      longitude = cfg.longitude;
      temperature = cfg.temperature;
      brightness  = cfg.brightness;
      tray = cfg.tray;
    } // cfg.extraOptions; # erlaubt z. B. fade, gamma, etc., falls Du sie brauchst
  };
}



