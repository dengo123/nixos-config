# modules/nixos/services/nvidia-pstated/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.nvidia-pstated;

  # build CLI args
  idsArg =
    if cfg.ids == null
    then []
    else ["-i" (concatStringsSep "," cfg.ids)];

  args =
    idsArg
    ++ ["--temperature-threshold" (toString cfg.temperatureThreshold)]
    ++ ["--performance-state-low" (toString cfg.performanceStateLow)]
    ++ ["--performance-state-high" (toString cfg.performanceStateHigh)]
    ++ ["--utilization-threshold" (toString cfg.utilizationThreshold)]
    ++ ["--iterations-before-switch" (toString cfg.iterationsBeforeSwitch)]
    ++ ["--iterations-before-idle" (toString cfg.iterationsBeforeIdle)]
    ++ ["--sleep-interval" (toString cfg.sleepIntervalMs)]
    ++ optional (cfg.disableFanScript != null) "--disable-fan-script"
    ++ optional (cfg.disableFanScript != null) cfg.disableFanScript
    ++ optional (cfg.enableFanScript != null) "--enable-fan-script"
    ++ optional (cfg.enableFanScript != null) cfg.enableFanScript;

  exec = "${pkgs.nvidia-pstated}/bin/nvidia-pstated";
in {
  options.${namespace}.services.nvidia-pstated = with types; {
    enable = mkBoolOpt false "Enable nvidia-pstated to manage NVIDIA GPU performance states.";

    # Which GPUs to manage (as in nvidia-smi indices). null = all.
    ids = mkOpt (nullOr (listOf str)) null "GPU ids to manage (e.g. [\"0\"] or [\"0\" \"1\"]).";

    # Defaults from upstream, but we’ll tune in hardware.nvidia for offload stability.
    temperatureThreshold = mkOpt int 80 "Enable high-fan state above this temperature (°C).";
    performanceStateLow = mkOpt int 8 "Low performance state value (default 8).";
    performanceStateHigh = mkOpt int 16 "High performance state value (default 16).";
    utilizationThreshold = mkOpt int 0 "Utilization threshold (%) to switch to high state.";
    iterationsBeforeSwitch = mkOpt int 30 "Iterations before switching to low state.";
    iterationsBeforeIdle = mkOpt int 9000 "Iterations before idling (fan logic).";
    sleepIntervalMs = mkOpt int 100 "Polling interval in milliseconds.";

    disableFanScript = mkOpt (nullOr str) null "Script to run to disable external GPU fans (optional).";
    enableFanScript = mkOpt (nullOr str) null "Script to run to enable external GPU fans (optional).";
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = pkgs ? nvidia-pstated;
        message = "${namespace}.services.nvidia-pstated: pkgs.nvidia-pstated is missing. Add the nvidia-pstated flake as an input and expose it via an overlay.";
      }
    ];

    systemd.services.nvidia-pstated = {
      description = "Automatically manage NVIDIA GPU performance states (nvidia-pstated)";
      wantedBy = ["multi-user.target"];
      after = ["multi-user.target" "systemd-udev-settle.service"];
      wants = ["systemd-udev-settle.service"];

      serviceConfig = {
        Type = "simple";
        ExecStart = "${exec} ${escapeShellArgs args}";
        Restart = "on-failure";
        RestartSec = "1s";
        DynamicUser = true;

        # Needs to talk to the driver/NVML; keep it host-level.
        # If permissions ever bite, we can add SupplementaryGroups later.
      };
    };
  };
}
