# modules/home/programs/nemo/default.nix
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
  cfg = config.${namespace}.programs.nemo;
in
{
  options.${namespace}.programs.nemo = with types; {
    enable = mkBoolOpt false "Enable Nemo (per-user via Home Manager).";

    # Komplettpaket aus Cinnamon (diverse Plugins)
    withBundle = mkBoolOpt false "Install nemo-with-extensions (bundle) in home.packages.";

    # Granular, wenn Bundle aus ist:
    extensions = {
      fileroller = mkBoolOpt true "nemo-fileroller (Archiv-Integration).";
      preview = mkBoolOpt true "nemo-preview (Quick Look).";
      emblems = mkBoolOpt false "nemo-emblems (Emblem-Overlay).";
      seahorse = mkBoolOpt false "nemo-seahorse (GnuPG/Keyring-Integration).";
      python = mkBoolOpt false "nemo-python (Python-API für eigene Actions/Plugins).";
      qmlDbus = mkBoolOpt false "nemo-qml-plugin-dbus (QML/DBus plugin).";
    };
  };

  config = mkIf cfg.enable {
    # Nur User-Pakete – GVFS/Polkit/udisks2 bitte systemseitig aktivieren!
    home.packages =
      (with pkgs; [
        nemo
      ])
      ++ (with pkgs; optional cfg.withBundle nemo-with-extensions)
      ++ (
        with pkgs;
        optionals (!cfg.withBundle && cfg.extensions.fileroller) [
          nemo-fileroller
          file-roller
        ]
      )
      ++ (with pkgs; optionals (!cfg.withBundle && cfg.extensions.preview) [ nemo-preview ])
      ++ (with pkgs; optionals (!cfg.withBundle && cfg.extensions.emblems) [ nemo-emblems ])
      ++ (with pkgs; optionals (!cfg.withBundle && cfg.extensions.seahorse) [ nemo-seahorse ])
      ++ (with pkgs; optionals (!cfg.withBundle && cfg.extensions.python) [ nemo-python ])
      ++ (with pkgs; optionals (!cfg.withBundle && cfg.extensions.qmlDbus) [ nemo-qml-plugin-dbus ]);

    home.sessionVariables.FILE_MANAGER = "nemo";
  };
}
