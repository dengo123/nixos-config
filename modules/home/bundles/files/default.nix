# modules/home/bundles/files/default.nix
{
  lib,
  config,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.bundles.files;
in {
  options.${namespace}.bundles.files = with types; {
    enable = mkBoolOpt false "Enable the files bundle.";

    manager =
      mkOpt (types.nullOr (
        types.enum [
          "nemo"
        ]
      ))
      null
      "Choose a GUI file manager. If null, no GUI file manager is enabled by this bundle.";

    archives = mkBoolOpt true "Install archive utilities.";
    appimage = mkBoolOpt true "Install AppImage support utilities.";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages =
        (optionals cfg.archives (with pkgs; [
          unrar
          unzip
          zip
          p7zip
        ]))
        ++ (optionals cfg.appimage (with pkgs; [
          appimage-run
          fuse
        ]));
    }

    (mkIf (cfg.manager == "nemo") {
      ${namespace}.programs.nemo.enable = true;
    })
  ]);
}
