# modules/home/services/xscreensaver/suspend.nix
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

  xssSuspend = pkgs.writeShellScript "xss-suspend.sh" ''
    set -eu
    TIMEOUT=${toString cfg.afterSeconds}
    PENDING=""
    ${pkgs.xscreensaver}/bin/xscreensaver-command -watch | while read -r line; do
      case "$line" in
        BLANK*|LOCK*)
          if [ -n "$PENDING" ] && kill -0 "$PENDING" 2>/dev/null; then
            kill "$PENDING" 2>/dev/null || true
          fi
          ( sleep "$TIMEOUT" && ${pkgs.systemd}/bin/systemctl suspend ) &
          PENDING=$!
          ;;
        UNBLANK*|UNLOCK*)
          if [ -n "$PENDING" ] && kill -0 "$PENDING" 2>/dev/null; then
            kill "$PENDING" 2>/dev/null || true
          fi
          ;;
      esac
    done
  '';
in {
  options.${namespace}.services.xscreensaver = with types; {
    enable = mkBoolOpt true "Suspend the system after xscreensaver has been blank for N seconds.";
    afterSeconds = mkOpt int 1800 "Seconds to wait after BLANK before suspending.";
  };

  config = mkIf cfg.enable {
    # xscreensaver muss laufen (du hast bereits ein HM-Modul dafür)
    systemd.user.services."xss-suspend" = {
      Unit = {
        Description = "Suspend after xscreensaver BLANK";
        After = ["graphical-session.target"];
        PartOf = ["graphical-session.target"];
      };
      Service = {
        Environment = [
          "DISPLAY=:0"
          "XAUTHORITY=%h/.Xauthority"
        ];
        ExecStart = xssSuspend;
        Restart = "always";
        RestartSec = 1;
      };
      Install = {
        WantedBy = ["graphical-session.target"];
      };
    };
  };
}
