{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.misc.scripts.auto-monitors;

  autoMonitorScript = pkgs.writeShellScriptBin "auto-monitors" ''
    #!/usr/bin/env bash

    monitors=$(hyprctl monitors -j | jq -r '.[].name')

    has() {
      echo "$monitors" | grep -q "$1"
    }

    set_monitor() {
      echo "Setting monitor: $1"
      hyprctl keyword monitor "$1"
    }

    # === Triple Monitor Setup (e.g. DP-1 + DP-2 + HDMI-A-1) ===
    if has "DP-1" && has "DP-2" && has "HDMI-A-1"; then
      set_monitor "DP-1,1920x1080@60,0x0,1"
      set_monitor "DP-2,1920x1080@60,1920x0,1"
      set_monitor "HDMI-A-1,1920x1080@60,3840x0,1"

    # === Dual Monitor Setup (DP-1 + DP-2) ===
    elif has "DP-1" && has "DP-2"; then
      set_monitor "DP-1,1920x1080@60,0x0,1"
      set_monitor "DP-2,1920x1080@60,1920x0,1"

    # === Laptop only ===
    elif has "eDP-1"; then
      set_monitor "eDP-1,preferred,auto,1"

    # === Single external display ===
    elif has "DP-1"; then
      set_monitor "DP-1,preferred,auto,1"

    elif has "HDMI-A-1"; then
      set_monitor "HDMI-A-1,preferred,auto,1"

    # === USB-C / Thunderbolt Display ===
    elif has "USB-C-1"; then
      set_monitor "USB-C-1,preferred,auto,1"
    fi
  '';
in {
  options.${namespace}.misc.scripts.auto-monitors = with types; {
    enable = mkBoolOpt false "Enable auto-monitors script";
  };

  config = mkIf cfg.enable {
    home.packages = [autoMonitorScript];
  };
}
