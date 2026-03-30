# modules/home/services/xidlehook/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.xidlehook;
in {
  options.${namespace}.services.xidlehook = with types; {
    enable = mkBoolOpt false "Enable xidlehook idle actions.";

    detectSleep = mkBoolOpt true "Reset idle timers after suspend/resume.";
    notWhenFullscreen = mkBoolOpt true "Do not trigger timers while fullscreen is active.";
    notWhenAudio = mkBoolOpt false "Do not trigger timers while audio is playing.";
    once = mkBoolOpt false "Run xidlehook once and exit.";

    environment = mkOpt (attrsOf str) {} "Extra environment variables exported for xidlehook commands.";

    timers = mkOpt (listOf (submodule {
      options = {
        delay = mkOpt ints.unsigned 60 "Time before executing the command.";
        command = mkOpt (nullOr str) null "Command executed after the idle timeout is reached.";
        canceller = mkOpt str "" "Command executed when user activity resumes before the next timer.";
      };
    })) [] "xidlehook timer chain.";
  };

  config = mkIf cfg.enable {
    services.xidlehook = {
      enable = true;
      detect-sleep = cfg.detectSleep;
      not-when-fullscreen = cfg.notWhenFullscreen;
      not-when-audio = cfg.notWhenAudio;
      once = cfg.once;
      environment = cfg.environment;
      timers = cfg.timers;
    };
  };
}
