# modules/nixos/services/security/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.security;

  polkitPowerRules = ''
    polkit.addRule(function(action, subject) {
      if (subject.isInGroup("users") && (
        action.id == "org.freedesktop.login1.reboot" ||
        action.id == "org.freedesktop.login1.reboot-multiple-sessions" ||
        action.id == "org.freedesktop.login1.power-off" ||
        action.id == "org.freedesktop.login1.power-off-multiple-sessions"
      )) { return polkit.Result.YES; }
    });
  '';

  polkitSuspendLockRules = ''
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
in {
  options.${namespace}.services.security = with types; {
    enable = mkBoolOpt true "Core security (polkit, rtkit)";
    allowPowerActions = mkBoolOpt false "Add permissive polkit rules for reboot/poweroff";
    allowSuspendLock = mkBoolOpt false "Allow suspend/lock via logind without auth (wheel/users)";
    greeterAfterResume = mkBoolOpt false "After resume, switch to LightDM greeter.";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      security = {
        polkit.enable = true;
        rtkit.enable = true;
        polkit.extraConfig = concatStringsSep "\n" [
          (optionalString cfg.allowPowerActions polkitPowerRules)
          (optionalString cfg.allowSuspendLock polkitSuspendLockRules)
        ];
      };
    }

    # Greeter nach Resume: via dm-tool (D-Bus), mit kurzem Retry, sauber unter `config`
    (mkIf cfg.greeterAfterResume {
      powerManagement.resumeCommands = ''
        ${pkgs.bash}/bin/bash -eu <<'EOF'
        DMTOOL=${pkgs.lightdm}/bin/dm-tool
        SH=${pkgs.bash}/bin/bash

        # 1) kurzes Settle, dann zum Greeter schalten (D-Bus, DISPLAY-frei)
        sleep 0.4
        i=0
        while [ $i -lt 15 ]; do
          if XDG_SEAT_PATH=/org/freedesktop/DisplayManager/Seat0 "$DMTOOL" switch-to-greeter 2>/dev/null; then
            break
          fi
          if "$DMTOOL" --seat seat0 switch-to-greeter 2>/dev/null; then
            break
          fi
          sleep 0.2
          i=$((i+1))
        done

        # 2) Greeter-X neu layouten (auch nach Hibernate), indem wir dein
        #    /etc/lightdm/display-setup.sh im Kontext des Greeter-X ausführen.
        #    Wir finden eine passende XAUTHORITY-Datei und laufen als 'lightdm'.
        run_in_greeter_x() {
          # Mögliche XAUTHORITY-Pfade je nach LightDM/Distribution:
          for XA in \
            /run/lightdm/root/:0 \
            /var/run/lightdm/root/:0 \
            /run/lightdm/greeter/Xauthority \
            /var/lib/lightdm/.Xauthority
          do
            if [ -r "$XA" ]; then
              DISPLAY=:0 XAUTHORITY="$XA" exec "$@"
              return 0
            fi
          done
          return 1
        }

        if [ -x /etc/lightdm/display-setup.sh ]; then
          # als lightdm-User ausführen (falls vorhanden), sonst root (best effort)
          if id lightdm >/dev/null 2>&1; then
            run_in_greeter_x su -s /bin/sh lightdm -c /etc/lightdm/display-setup.sh \
              || run_in_greeter_x /etc/lightdm/display-setup.sh || true
          else
            run_in_greeter_x /etc/lightdm/display-setup.sh || true
          fi
        fi

        exit 0
        EOF
      '';
    })
  ]);
}
