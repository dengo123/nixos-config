# modules/programs/nemo/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.nemo;
in {
  options.${namespace}.programs.nemo = with types; {
    enable = mkBoolOpt false "Enable Nemo file manager (GTK).";

    # Falls du einfach das Cinnamon-Bundle willst:
    withBundle = mkBoolOpt false "Install nemo-with-extensions (bundled common plugins).";

    # Feingranular, nur wenn withBundle = false:
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
    # Wichtig für trash://, smb://, sftp://, mtp:// etc.
    services.gvfs.enable = true;

    environment = {
      systemPackages =
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
        ++ (with pkgs; optionals (!cfg.withBundle && cfg.extensions.preview) [nemo-preview])
        ++ (with pkgs; optionals (!cfg.withBundle && cfg.extensions.emblems) [nemo-emblems])
        ++ (with pkgs; optionals (!cfg.withBundle && cfg.extensions.seahorse) [nemo-seahorse])
        ++ (with pkgs; optionals (!cfg.withBundle && cfg.extensions.python) [nemo-python])
        ++ (with pkgs; optionals (!cfg.withBundle && cfg.extensions.qmlDbus) [nemo-qml-plugin-dbus]);

      # macht systemweit Nemo-Actions (.nemo_action) sichtbar, falls Pakete welche bereitstellen
      pathsToLink = ["/share/nemo/actions"];
    };
  };
}
