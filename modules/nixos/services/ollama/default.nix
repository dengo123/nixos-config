{
  config,
  lib,
  namespace,
  pkgs,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.ollama;
in {
  options.${namespace}.services.ollama = {
    enable = mkBoolOpt false "Enable the Ollama service.";
    acceleration = mkOption {
      type = types.enum ["cuda" "rocm" "cpu"];
      default = "cuda";
      description = "Acceleration backend to use (e.g. CUDA for NVIDIA GPUs).";
    };
    port = mkOption {
      type = types.port;
      default = 11434;
      description = "Port on which Ollama will listen.";
    };
  };

  config = mkIf cfg.enable {
    services.ollama = {
      enable = true;
      acceleration = cfg.acceleration;
      port = cfg.port;
      openFirewall = true;
      package = pkgs.ollama;
    };
  };
}
