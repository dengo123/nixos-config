# modules/home/services/picom/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.picom;
in {
  options.${namespace}.services.picom = with types; {
    enable = mkBoolOpt false "Enable Picom integration.";

    package = mkOpt package pkgs.picom "Picom package to use.";

    manageConfig = mkOpt bool true ''
      If true, Home Manager manages ~/.config/picom/picom.conf and starts Picom.
      If false, config management and autostart are expected to be handled externally.
    '';

    configFile =
      mkOpt (types.nullOr types.path) null
      "Path to picom.conf when Home Manager manages the config.";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = [cfg.package];
    }

    (mkIf cfg.manageConfig {
      assertions = [
        {
          assertion = cfg.configFile != null;
          message = "${namespace}.services.picom.configFile must be set when manageConfig is enabled.";
        }
      ];

      xdg.configFile."picom/picom.conf".source = cfg.configFile;

      services.picom = {
        enable = true;
        package = cfg.package;
        extraArgs = [
          "--config"
          "${config.xdg.configHome}/picom/picom.conf"
        ];
      };
    })
  ]);
}
