# modules/home/services/xidlehook/default.nix
# => umgewidmet zu: XScreensaver + Lock-Watcher (LightDM)
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.xscreensaver;

  # Locker-Kommando (LightDM)
  lockerCmd = cfg.lockerCmd or "${pkgs.lightdm}/bin/dm-tool lock";

  # Watcher-Skript: wartet auf XScreensaver-ACTIVATE und lockt nach delay
  xsaverWatcher = pkgs.writeShellScript "xsaver-lock-watch.sh" ''
    set -eu
    LOCK_DELAY="${toString cfg.delaySeconds}"
    LOCK_CMD='${lockerCmd}'

    cancel_pending() {
      # alle schlafenden "sleep ...; LOCK_CMD"-Jobs beenden
      pkill -f "sleep ${toString cfg.delaySeconds}; ${escapeShellArg lockerCmd}" 2>/dev/null || true
    }

    # Ereignisse verfolgen
    exec ${pkgs.xscreensaver}/bin/xscreensaver-command -watch | while read -r line; do
      case "$line" in
        *ACTIVATE*)
          # geplante Locks säubern, neuen verzögerten Lock einplanen
          cancel_pending
          ( sleep "$LOCK_DELAY"; ${lockerCmd} ) &
          ;;
        *DEACTIVATE*)
          # Benutzerinteraktion -> geplante Locks abbrechen
          cancel_pending
          ;;
      esac
    done
  '';
in {
  options.${namespace}.services.xscreensaver = with types; {
    enable = mkBoolOpt false "XScreensaver als Bildschirmschoner mit LightDM-Autolock nach Verzögerung.";
    # Mindestzeit, die der Saver sichtbar ist, bevor gelockt wird (Sekunden)
    delaySeconds =
      mkOpt ints.positive 300
      "Verzögerung (Sek.) zwischen XScreensaver-ACTIVATE und Lock.";
    # Locker-Kommando (Standard: LightDM dm-tool lock). Alternative: light-locker-command -l
    lockerCmd =
      mkOpt str null
      "Lock-Kommando (z. B. '${pkgs.lightdm}/bin/dm-tool lock' oder '${pkgs.lightlocker}/bin/light-locker-command -l').";
  };

  config = mkIf cfg.enable {
    # XScreensaver-Dienst: nur Saver, kein eingebauter Lock
    services.xscreensaver = {
      enable = true;
      package = pkgs.xscreensaver;
      # Tipp: Stelle in der GUI/ ~/.xscreensaver sicher: lock: False, timeout >= 5m
      # (Wir locken separat per Watcher)
    };

    # Watcher als systemd-user service
    systemd.user.services.xsaver-lock-watch = {
      Unit = {
        Description = "Lock LightDM nach XScreensaver-Aktivierung (verzögert)";
        After = ["graphical-session.target"];
        PartOf = ["graphical-session.target"];
      };
      Service = {
        ExecStart = xsaverWatcher;
        Environment = [
          # DISPLAY/XAUTHORITY absichern – passe an, falls nötig
          "DISPLAY=:0"
          "XAUTHORITY=${config.home.homeDirectory}/.Xauthority"
        ];
        Restart = "always";
      };
      Install = {
        WantedBy = ["graphical-session.target"];
      };
    };

    # Nützliche Tools im PATH
    home.packages = [
      pkgs.xscreensaver
      pkgs.lightlocker
      pkgs.procps # für pkill
    ];
  };
}
