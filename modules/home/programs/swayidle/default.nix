{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.swayidle;
in {
  options.${namespace}.programs.swayidle = with types; {
    enable = mkBoolOpt false "Enable swayidle for session idle management";
    lockCommand = mkStrOpt "swaylock -f -c 000000" "Command to lock screen";
  };

  config = mkIf cfg.enable {
    services.swayidle = {
      enable = true;
      timeouts = [
        {
          timeout = 600;
          command = cfg.lockCommand;
        }
        {
          timeout = 900;
          command = "swaymsg 'output * dpms off'";
          resumeCommand = "swaymsg 'output * dpms on'";
        }
        {
          timeout = 1800;
          command = "systemctl suspend";
        }
        {
          timeout = 3600;
          command = "systemctl hibernate";
        }
      ];
      events = [
        {
          event = "before-sleep";
          command = cfg.lockCommand;
        }
        {
          event = "after-resume";
          command = "loginctl lock-session";
        }
      ];
    };
  };
}
