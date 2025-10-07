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
    enable = mkBoolOpt false "Core security (polkit, rtkit)";
    allowPowerActions = mkBoolOpt false "Add permissive polkit rules for reboot/poweroff";
    allowSuspendLock = mkBoolOpt true "Allow suspend/lock via logind without auth (wheel/users)";
    greeterAfterResume = mkBoolOpt true "After resume, switch to LightDM greeter.";
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

        # kurzes Settle nach dem Aufwachen
        sleep 0.4

        # bis zu ~3s versuchen (15*0.2s), falls LightDM noch nicht lauscht
        i=0
        while [ $i -lt 15 ]; do
          # Variante 1: expliziter Seat-Pfad
          if XDG_SEAT_PATH=/org/freedesktop/DisplayManager/Seat0 "$DMTOOL" switch-to-greeter 2>/dev/null; then
            exit 0
          fi
          # Variante 2: --seat seat0
          if "$DMTOOL" --seat seat0 switch-to-greeter 2>/dev/null; then
            exit 0
          fi
          sleep 0.2
          i=$((i+1))
        done

        # kein harter Fehler, einfach weiter
        exit 0
        EOF
      '';
    })
  ]);
}
