{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.kanshi;
in {
  options.${namespace}.services.kanshi = {
    enable = mkEnableOption "Enable kanshi for automatic monitor profile switching";

    # Optional: Override kanshi package
    package = mkOption {
      type = types.package;
      default = pkgs.kanshi;
      description = "The kanshi package to use.";
    };
  };

  config = mkIf cfg.enable {
    services.kanshi = {
      enable = true;
      package = cfg.package;

      profiles = {
        dual = {
          outputs = [
            {
              # Hochkant-Monitor (links / Hauptmonitor)
              criteria = "Hewlett Packard HP E232 3CQ7020B20";
              mode = "1920x1080@60";
              position = "0,0";
              transform = "90"; # 90° Rotation
            }
            {
              # Quer-Monitor (rechts)
              criteria = "Hewlett Packard HP E232 3CQ70218NK";
              mode = "1920x1080@60";
              position = "1080,420";
            }
          ];
        };
      };
    };
  };
}
