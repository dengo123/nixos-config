{
  config,
  lib,
  namespace,
  pkgs,
  ...
}:
with lib;
with lib.${namespace};
let
  cfg = config.${namespace}.services.ollama;
in
{
  options.${namespace}.services.ollama = {
    enable = mkBoolOpt false "Enable the Ollama service.";

    package = mkOpt types.package pkgs.ollama ''
      The Ollama package to use, e.g.:
        pkgs.ollama
        pkgs.ollama-cuda
        pkgs.ollama-rocm
        pkgs.ollama-vulkan
        pkgs.ollama-cpu
    '';

    port = mkOption {
      type = types.port;
      default = 11434;
      description = "Port on which Ollama will listen.";
    };
  };

  config = mkIf cfg.enable {
    services.ollama = {
      enable = true;
      package = cfg.package;
      port = cfg.port;
      openFirewall = true;
    };
  };
}
