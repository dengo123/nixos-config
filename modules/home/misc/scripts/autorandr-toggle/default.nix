# modules/home/misc/scripts/autorandr-toggle/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.misc.scripts.autorandr-toggle;

  script =
    pkgs.writeShellScriptBin "autorandr-toggle"
    ''
      set -euo pipefail

      WORK="${cfg.workProfile}"
      GAME="${cfg.gameProfile}"

      have() { command -v "$1" >/dev/null 2>&1; }

      notify() {
        if have notify-send; then
          notify-send "autorandr-toggle" "$1" || true
        fi
      }

      awesome_sig() {
        # best effort: nur wenn awesome-client existiert und wir in X sind
        if have awesome-client; then
          awesome-client "awesome.emit_signal(\"$1\")" >/dev/null 2>&1 || true
        fi
      }

      cur="$(autorandr --current 2>/dev/null | head -n1 | tr -d '\r' || true)"

      if [ -z "$cur" ]; then
        # fallback: autorandr kann --current manchmal leer liefern (z.B. wenn config nicht 1:1 matcht)
        cur="$(autorandr --detected 2>/dev/null | head -n1 | tr -d '\r' || true)"
      fi

      if [ "$cur" = "$GAME" ]; then
        next="$WORK"
      else
        next="$GAME"
      fi

      awesome_sig "autorandr::pre"

      autorandr --load "$next" --force

      sleep 0.15

      awesome_sig "autorandr::applied"
    '';
in {
  options.${namespace}.misc.scripts.autorandr-toggle = with types; {
    enable = mkBoolOpt false "Enable misc.scripts.autorandr-toggle";
    workProfile = mkStrOpt "dual" "Autorandr profile name for WORK layout.";
    gameProfile = mkStrOpt "tv" "Autorandr profile name for GAMING layout.";
  };

  config = mkIf cfg.enable {
    home.packages = [
      pkgs.autorandr
      pkgs.xorg.xrandr
      script
    ];
  };
}
