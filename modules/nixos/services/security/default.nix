{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.security;
in {
  options.${namespace}.services.security = with types; {
    enable = mkBoolOpt false "Core security (polkit, rtkit)";
    allowPowerActions = mkBoolOpt false "Add permissive polkit rules for reboot/poweroff";
  };

  config = mkIf cfg.enable {
    security = {
      polkit.enable = true;
      rtkit.enable = true; # gut für PipeWire/JACK
      polkit.extraConfig = mkIf cfg.allowPowerActions ''
        polkit.addRule(function(action, subject) {
          if (subject.isInGroup("users") && (
            action.id == "org.freedesktop.login1.reboot" ||
            action.id == "org.freedesktop.login1.reboot-multiple-sessions" ||
            action.id == "org.freedesktop.login1.power-off" ||
            action.id == "org.freedesktop.login1.power-off-multiple-sessions"
          )) { return polkit.Result.YES; }
        });
      '';
      pam.services.xscreensaver.enable = true;
    };
  };
}
