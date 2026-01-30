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

  # single source of truth: bundles.gpu.vendor
  gpuVendor = (config.${namespace}.bundles.gpu.vendor or null);

  # pick ollama package by vendor
  ollamaPkg =
    if gpuVendor == "nvidia" then
      pkgs.ollama-cuda
    else if gpuVendor == "amd" then
      pkgs.ollama-rocm
    else
      pkgs.ollama-cpu;
in
{
  options.${namespace}.bundles.ai = with types; {
    enable = mkBoolOpt false "Enable bundles.ai";
  };

  config = mkIf cfg.enable (mkMerge [
    # base AI tooling + ollama enable
    {
      environment.systemPackages = with pkgs; [
        whisper-cpp
        whisper-cpp-vulkan
      ];

      ${namespace} = {
        services.ollama = {
          enable = true;
          package = ollamaPkg;
        };
      };
    }

    # NVIDIA -> turn on CUDA wiring
    (mkIf (gpuVendor == "nvidia") {
      ${namespace}.hardware.cuda = enabled;
    })
  ]);
}
