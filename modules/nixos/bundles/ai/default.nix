# modules/nixos/bundles/ai/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace};
let
  cfg = config.${namespace}.bundles.ai;

  gpuVendor = (config.${namespace}.bundles.gpu.vendor or null);

  ollamaPkg =
    if gpuVendor == "nvidia" then
      pkgs.ollama-cuda
    else if gpuVendor == "amd" then
      pkgs.ollama-rocm
    else
      pkgs.ollama-cpu;

  whisperBackend =
    if gpuVendor == "nvidia" then
      "cuda"
    else if gpuVendor == "amd" then
      "vulkan"
    else
      "cpu";
in
{
  options.${namespace}.bundles.ai = with types; {
    enable = mkBoolOpt false "Enable bundles.ai";
  };

  config = mkIf cfg.enable {
    ${namespace} = {
      # Whisper als Modul aktivieren
      programs.whisper = {
        enable = true;
        backend = whisperBackend;
        # modelPath/rootDir bleiben defaults aus dem whisper-modul,
        # oder hier überschreiben.
      };

      services.ollama = {
        enable = true;
        package = ollamaPkg;
      };
    };
  };
}
