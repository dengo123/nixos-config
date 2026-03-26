# modules/nixos/services/polkit/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.polkit;

  polkitPowerRules = ''
    polkit.addRule(function(action, subject) {
      if (subject.isInGroup("wheel") && (
        action.id == "org.freedesktop.login1.reboot" ||
        action.id == "org.freedesktop.login1.reboot-multiple-sessions" ||
        action.id == "org.freedesktop.login1.power-off" ||
        action.id == "org.freedesktop.login1.power-off-multiple-sessions"
      )) {
        return polkit.Result.YES;
      }
    });
  '';

  polkitSleepRules = ''
    polkit.addRule(function(action, subject) {
      const acts = [
        "org.freedesktop.login1.suspend",
        "org.freedesktop.login1.suspend-multiple-sessions",
        "org.freedesktop.login1.hibernate",
        "org.freedesktop.login1.hibernate-multiple-sessions",
        "org.freedesktop.login1.hybrid-sleep",
        "org.freedesktop.login1.hybrid-sleep-multiple-sessions",
        "org.freedesktop.login1.suspend-then-hibernate",
        "org.freedesktop.login1.suspend-then-hibernate-multiple-sessions",
        "org.freedesktop.login1.lock-session",
        "org.freedesktop.login1.lock-sessions",
        "org.freedesktop.UPower.Suspend",
        "org.freedesktop.UPower.Hibernate"
      ];

      if (subject.isInGroup("wheel") &&
          acts.indexOf(action.id) >= 0) {
        return polkit.Result.YES;
      }
    });
  '';
in {
  options.${namespace}.services.polkit = with types; {
    enable = mkBoolOpt true "Enable polkit and rtkit integration.";
    allowPowerActions = mkBoolOpt false "Allow reboot and poweroff without additional authentication.";
    allowSleepActions = mkBoolOpt true "Allow suspend, hibernate, hybrid sleep, suspend-then-hibernate, and lock without additional authentication.";
  };

  config = mkIf cfg.enable {
    security = {
      polkit.enable = true;
      rtkit.enable = true;

      polkit.extraConfig = concatStringsSep "\n" (
        filter (x: x != "") [
          (optionalString cfg.allowPowerActions polkitPowerRules)
          (optionalString cfg.allowSleepActions polkitSleepRules)
        ]
      );
    };
  };
}
