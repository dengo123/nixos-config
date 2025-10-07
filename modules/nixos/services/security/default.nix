# modules/nixos/services/security/default.nix
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
    allowSuspendLock = mkBoolOpt true "Allow suspend/lock via logind without auth (wheel/users)";
    greeterAfterResume = mkBoolOpt true "After resume, lock sessions so LightDM greeter shows up.";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      security = {
        polkit.enable = true;
        rtkit.enable = true; # gut für PipeWire/JACK
      };
    }

    # Reboot/Poweroff ohne Passwort (deine bestehende Regel)
    (mkIf cfg.allowPowerActions {
      security.polkit.extraConfig = ''
        polkit.addRule(function(action, subject) {
          if (subject.isInGroup("users") && (
            action.id == "org.freedesktop.login1.reboot" ||
            action.id == "org.freedesktop.login1.reboot-multiple-sessions" ||
            action.id == "org.freedesktop.login1.power-off" ||
            action.id == "org.freedesktop.login1.power-off-multiple-sessions"
          )) { return polkit.Result.YES; }
        });
      '';
    })

    # Suspend/Lock ohne Passwort (wichtig für deine User-Services)
    (mkIf cfg.allowSuspendLock {
      security.polkit.extraConfig = ''
        polkit.addRule(function(action, subject) {
          const acts = [
            "org.freedesktop.login1.suspend",
            "org.freedesktop.login1.suspend-multiple-sessions",
            "org.freedesktop.login1.lock-session",
            "org.freedesktop.login1.lock-sessions"
          ];
          if ((subject.isInGroup("wheel") || subject.isInGroup("users")) &&
              acts.indexOf(action.id) >= 0) {
            return polkit.Result.YES;
          }
        });
      '';
    })

    # Nach Resume Greeter erzwingen (root, robust, kein DISPLAY nötig)
    (mkIf cfg.greeterAfterResume {
      systemd.services."greeter-after-resume" = {
        description = "Lock all sessions after resume so LightDM greeter shows";
        wantedBy = ["sleep.target"];
        after = ["sleep.target"];
        # Hinweis: Bei sleep.target wird 'start' vor dem Schlaf gerufen,
        # 'stop' nach dem Resume -> also ExecStop zum Sperren verwenden.
        serviceConfig = {
          Type = "oneshot";
          ExecStop = "${config.systemd.package or pkgs.systemd}/bin/loginctl lock-sessions";
        };
      };
    })
  ]);
}
