# modules/nixos/hardware/audio/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.hardware.audio;
in {
  options.${namespace}.hardware.audio = with types; {
    enable = mkBoolOpt false "Enable PipeWire audio.";

    alsa = mkBoolOpt true "Enable ALSA support via PipeWire.";
    alsa32Bit = mkBoolOpt true "Enable 32-bit ALSA support for games and legacy applications.";
    pulse = mkBoolOpt true "Enable PulseAudio compatibility via PipeWire.";
    jack = mkBoolOpt false "Enable JACK compatibility via PipeWire.";
    gamingBuffer = mkBoolOpt false "Enable PipeWire settings tuned for gaming workloads.";
  };

  config = mkIf cfg.enable {
    services.pipewire = {
      enable = true;

      alsa = {
        enable = cfg.alsa;
        support32Bit = cfg.alsa32Bit;
      };

      pulse.enable = cfg.pulse;

      jack.enable = cfg.jack;

      extraConfig = mkIf cfg.gamingBuffer {
        pipewire."10-gaming-buffer" = {
          "context.properties" = {
            "default.clock.rate" = 48000;
            "default.clock.quantum" = 256;
            "default.clock.min-quantum" = 256;
            "default.clock.max-quantum" = 1024;
          };
        };
      };
    };
  };
}
