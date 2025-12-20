# modules/home/programs/redshift/default.nix
{
  lib,
  pkgs,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace};
let
  cfg = config.${namespace}.services.redshift;
in
{
  options.${namespace}.services.redshift = with types; {
    enable = mkBoolOpt false "Enable redshift via services.redshift.";
    package = mkOpt types.package pkgs.redshift "Redshift-Paket.";

    # Standortquelle
    provider = mkOpt (types.enum [
      "manual"
      "geoclue2"
    ]) "manual" "Standortquelle.";
    latitude = mkOpt (types.nullOr types.number) null "Breitengrad (bei provider=manual).";
    longitude = mkOpt (types.nullOr types.number) null "Längengrad (bei provider=manual).";

    # Farbtemperaturen (K)
    temperature =
      mkOpt
        (types.nullOr (
          types.submodule {
            options.day = mkOpt types.int 5500 "Tag (K)";
            options.night = mkOpt types.int 3800 "Nacht (K)";
          }
        ))
        {
          day = 5500;
          night = 3800;
        }
        "Farbtemperaturen in Kelvin.";

    # Helligkeit (Multiplikator 0..1)
    settings.brightness =
      mkOpt
        (types.nullOr (
          types.submodule {
            options.day = mkOpt types.number 1.0 "Tag (0–1)";
            options.night = mkOpt types.number 0.9 "Nacht (0–1)";
          }
        ))
        {
          day = 1.0;
          night = 0.9;
        }
        "Helligkeit (Multiplikator).";

    tray = mkBoolOpt false "Tray-Icon aktivieren (falls vom Desktop unterstützt).";

    # Freie Durchreichung zusätzlicher HM-Optionen (z. B. fade/gamma etc.)
    extraOptions =
      mkOpt (types.attrsOf types.anything) { }
        "Weitere HM-Optionen direkt an services.redshift durchreichen.";
  };

  config = mkIf cfg.enable (mkMerge [
    # Sanity: bei provider=manual müssen lat/long da sein
    {
      assertions = [
        {
          assertion = cfg.provider != "manual" || (cfg.latitude != null && cfg.longitude != null);
          message = "${namespace}.programs.redshift: provider=manual benötigt latitude und longitude.";
        }
      ];
    }

    # Neue HM-Syntax (vermeidet die Rename-Warnings)
    {
      services.redshift = {
        enable = true;
        package = cfg.package;
        tray = cfg.tray;

        # Standort
        provider = cfg.provider;
        latitude = cfg.latitude;
        longitude = cfg.longitude;

        # Neue Settings-Struktur
        settings = {
          redshift = {
            temperature-day = cfg.temperature.day;
            temperature-night = cfg.temperature.night;
            brightness-day = cfg.settings.brightness.day;
            brightness-night = cfg.settings.brightness.night;
          };
        };
      }
      // cfg.extraOptions;
    }
  ]);
}
