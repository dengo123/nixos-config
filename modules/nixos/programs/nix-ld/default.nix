# modules/nixos/programs/nix-ld/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.nix-ld;

  gpuVendor = config.${namespace}.bundles.gpu.vendor or null;

  nvidiaPackage = attrByPath ["hardware" "nvidia" "package"] null config;

  baseLibraries = with pkgs; [
    stdenv.cc.cc.lib
    zlib
  ];

  autoLibraries =
    baseLibraries
    ++ optionals (gpuVendor == "nvidia" && nvidiaPackage != null) [
      nvidiaPackage
    ];
in {
  options.${namespace}.programs.nix-ld = with types; {
    enable = mkBoolOpt false "Enable nix-ld for foreign dynamic binaries.";

    libraries = mkOpt (listOf package) [] "Extra libraries exposed through nix-ld.";

    useSteamRunSet = mkBoolOpt false "Use the broad steam-run library set for maximum binary compatibility.";
  };

  config = mkIf cfg.enable {
    programs.nix-ld = {
      enable = true;
      libraries =
        if cfg.useSteamRunSet
        then pkgs.steam-run.args.multiPkgs pkgs
        else autoLibraries ++ cfg.libraries;
    };
  };
}
