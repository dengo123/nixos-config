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
  xs = config.${namespace}.services.xscreensaver;

  xssSuspend = pkgs.writeShellScript "xss-suspend.sh" ''
    set -eu
    TIMEOUT=${toString xs.suspend.afterSeconds}
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
  options.${namespace}.services.xscreensaver.suspend = with types; {
    enable = mkBoolOpt true "Suspend after the screensaver has been active for a while.";
    afterSeconds = mkOpt int 1500 "Seconds to wait after saver activation before suspending.";
  };

  config = mkIf (xs.enable && xs.suspend.enable) {
    systemd.user.services."xss-suspend" = {
      Unit = {
        Description = "Suspend after xscreensaver active for ${toString xs.suspend.afterSeconds}s";
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
