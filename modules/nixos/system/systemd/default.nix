# modules/nixos/system/systemd/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.system.systemd;
in {
  options.${namespace}.system.systemd = with types; {
    enable = mkBoolOpt false "Enable hardened global systemd defaults.";

    defaultTimeoutStopSec =
      mkOpt str "15s"
      "Default timeout for stopping services.";

    logLevel = mkOpt (enum [
      "emerg"
      "alert"
      "crit"
      "err"
      "warning"
      "notice"
      "info"
      "debug"
    ]) "info" "Maximum journald log level to store.";

    logRateLimitInterval =
      mkOpt str "30s"
      "journald rate limit interval.";

    logRateLimitBurst =
      mkOpt int 200
      "journald rate limit burst.";

    systemMaxUse =
      mkOpt str "1G"
      "Maximum disk space journald may use.";

    maxRetentionSec =
      mkOpt str "1month"
      "Maximum time journald should retain logs.";

    persistentLogging =
      mkBoolOpt true
      "Store journald logs persistently on disk.";

    forwardToSyslog =
      mkBoolOpt false
      "Forward journald logs to syslog.";
  };

  config = mkIf cfg.enable {
    systemd.settings.Manager = {
      DefaultTimeoutStopSec = cfg.defaultTimeoutStopSec;
    };

    services.journald.extraConfig = ''
      Storage=${
        if cfg.persistentLogging
        then "persistent"
        else "auto"
      }
      Compress=yes
      Seal=yes
      RateLimitIntervalSec=${cfg.logRateLimitInterval}
      RateLimitBurst=${toString cfg.logRateLimitBurst}
      SystemMaxUse=${cfg.systemMaxUse}
      MaxRetentionSec=${cfg.maxRetentionSec}
      MaxLevelStore=${cfg.logLevel}
      ForwardToSyslog=${
        if cfg.forwardToSyslog
        then "yes"
        else "no"
      }
    '';
  };
}
