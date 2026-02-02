# modules/nixos/programs/nix-ld/default.nix (dein modul)
{
  config,
  inputs,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace};
let
  cfg = config.${namespace}.programs.nix-ld;
in
{
  options.${namespace}.programs.nix-ld = {
    enable = mkBoolOpt false "${namespace}.programs.nix-ld.enable";
  };

  config = mkIf cfg.enable {
    programs.nix-ld = {
      enable = true;

      # das ist der wichtige Teil:
      libraries = with pkgs; [
        stdenv.cc.cc.lib
        zlib

        # NVIDIA user-space driver libs (libcuda.so, libnvidia-ml.so, etc.)
        config.hardware.nvidia.package
      ];
    };
  };
}
