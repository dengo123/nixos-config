{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.hypridle;
in {
  options.${namespace}.programs.hypridle = with types; {
    enable = mkBoolOpt false "Enable programs.hypridle";
  };

  config = mkIf cfg.enable {
    services.hypridle = {
      enable = true;
      settings = {
        general = {
          ignore_dbus_inhibit = false;
          before_sleep_cmd = "hyprlock";
          after_sleep_cmd = "loginctl lock-session";
        };

        listener = [
          {
            timeout = 600;
            on-timeout = "hyprlock";
          }

          {
            timeout = 900;
            on-timeout = "hyprctl dispatch dpms off";
            on-resume = "hyprctl dispatch dpms on";
          }

          {
            timeout = 1800;
            on-timeout = "systemctl suspend";
          }
        ];
      };
    };
  };
}
