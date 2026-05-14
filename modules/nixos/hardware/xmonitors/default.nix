# modules/nixos/hardware/xmonitors/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.hardware.xmonitors;

  outputModule = types.submodule ({name, ...}: {
    options = {
      enable = mkBoolOpt true "Enable this output in the startup layout.";
      off = mkBoolOpt false "Force this output off.";

      primary = mkBoolOpt false "Mark this output as primary.";

      mode = mkOpt (types.nullOr types.str) null "Preferred mode, e.g. 1920x1080.";
      rate = mkOpt (types.nullOr types.int) null "Preferred refresh rate.";
      pos = mkOpt (types.nullOr types.str) null "Absolute position, e.g. 1080x420.";
      rotate = mkOpt (types.enum [
        "normal"
        "left"
        "right"
        "inverted"
      ]) "normal" "Output rotation.";

      scale = mkOpt (types.nullOr types.str) null "Optional xrandr scale, e.g. 1x1.";
    };
  });

  renderOutput = name: ocfg: let
    args =
      []
      ++ ["--output" name]
      ++ optional ocfg.off "--off"
      ++ optionals (!ocfg.off && ocfg.enable) (
        optional ocfg.primary "--primary"
        ++ optional (ocfg.mode != null) "--mode"
        ++ optional (ocfg.mode != null) ocfg.mode
        ++ optional (ocfg.rate != null) "--rate"
        ++ optional (ocfg.rate != null) (toString ocfg.rate)
        ++ optional (ocfg.pos != null) "--pos"
        ++ optional (ocfg.pos != null) ocfg.pos
        ++ ["--rotate" ocfg.rotate]
        ++ optional (ocfg.scale != null) "--scale"
        ++ optional (ocfg.scale != null) ocfg.scale
      );
  in
    escapeShellArgs args;

  configuredOutputs = attrNames cfg.outputs;

  xsetup = pkgs.writeShellScript "dm-xsetup" ''
        #!/usr/bin/env bash
        set -euo pipefail

        XR="${pkgs.xrandr}/bin/xrandr"
        GREP="${pkgs.gnugrep}/bin/grep"
        AWK="${pkgs.gawk}/bin/awk"
        SLEEP="${pkgs.coreutils}/bin/sleep"
        SEQ="${pkgs.coreutils}/bin/seq"

        export DISPLAY=":0"

        log() {
          echo "dm-xsetup: $*" >&2
        }

        connected() {
          "$XR" --query | "$GREP" -q "^$1 connected"
        }

        all_connected_outputs() {
          "$XR" --query | "$AWK" '/ connected/ { print $1 }'
        }

        ${
      optionalString cfg.waitForOutputs.enable ''
        for i in $("$SEQ" 1 ${toString cfg.waitForOutputs.retries}); do
          if "$XR" --query >/dev/null 2>&1 && "$XR" --query | "$GREP" -q " connected"; then
            break
          fi
          "$SLEEP" ${toString cfg.waitForOutputs.interval}
        done
      ''
    }

    ${optionalString (cfg.framebuffer.size != null) ''
      "$XR" --fb ${cfg.framebuffer.size} || true
    ''}

        ${
      concatStringsSep "\n" (
        map
        (name: let
          ocfg = cfg.outputs.${name};
        in ''
          if connected "${name}"; then
            log "applying output ${name}"
            "$XR" ${renderOutput name ocfg} || true
          else
            log "output ${name} not connected"
          fi
        '')
        configuredOutputs
      )
    }

        ${
      optionalString cfg.disableOthers ''
        for out in $(all_connected_outputs); do
          case "$out" in
            ${concatStringsSep "|" configuredOutputs})
              ;;
            *)
              log "disabling unmanaged output $out"
              "$XR" --output "$out" --off || true
              ;;
          esac
        done
      ''
    }

        exit 0
  '';
in {
  options.${namespace}.hardware.xmonitors = with types; {
    enable = mkBoolOpt false "Apply a host-specific X11 startup monitor layout.";

    waitForOutputs = {
      enable = mkBoolOpt true "Wait briefly until X enumerates outputs.";
      retries = mkOpt int 30 "How often to poll for outputs.";
      interval = mkOpt float 0.2 "Delay between output polling attempts.";
    };

    framebuffer = {
      size = mkOpt (nullOr str) null "Framebuffer size to request via xrandr --fb.";
    };

    disableOthers = mkBoolOpt true "Disable connected outputs not explicitly managed by this layout.";

    outputs = mkOpt (attrsOf outputModule) {} "Startup layout outputs keyed by xrandr connector name.";

    script = mkOpt package xsetup "Generated xrandr setup script.";
  };

  config = mkIf cfg.enable {
    services.xserver.enable = mkDefault true;

    assertions = [
      {
        assertion = cfg.outputs != {};
        message = "${namespace}.hardware.xmonitors.enable is true, but no outputs are configured.";
      }
    ];
  };
}
