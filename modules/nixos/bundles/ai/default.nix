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

  config = mkIf cfg.enable (mkMerge [
    # enable whisper + configure backend by vendor
    {
      ${namespace}.programs.whisper = {
        enable = true;
        backend = whisperBackend;
        # modelPath/rootDir defaults kommen schon aus dem whisper-modul,
        # kannst du hier aber überschreiben, wenn du willst.
      };

      ${namespace}.services.ollama = {
        enable = true;
        package = ollamaPkg;
      };
    }

    # only needed for nvidia/cuda backend:
    (mkIf (gpuVendor == "nvidia") {
      nixpkgs.overlays = [
        (import ../../../overlays/whisper-cpp-cuda) # Pfad ggf. anpassen!
      ];
    })
  ]);
}
