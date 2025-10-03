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

    extensions = {
      fileroller = mkBoolOpt true "Enable Nemo integration with File Roller (extract/compress).";
      preview = mkBoolOpt true "Enable nemo-preview (quick look).";
      imageTools = mkBoolOpt true "Enable image converter/rotator actions for Nemo.";
      terminal = mkBoolOpt false "Enable embedded terminal plugin for Nemo (nemo-terminal).";
    };
  };

  config = mkIf cfg.enable {
    # Für Trash://, SMB, SFTP, MTP usw.
    services.gvfs.enable = true;

    environment = {
      systemPackages =
        (with pkgs; [
          nemo
        ])
        ++ (with pkgs; optional cfg.extensions.fileroller file-roller)
        ++ (with pkgs; optional cfg.extensions.fileroller nemo-fileroller)
        ++ (with pkgs; optional cfg.extensions.preview nemo-preview)
        ++ (with pkgs; optional cfg.extensions.imageTools nemo-image-converter)
        ++ (with pkgs; optional cfg.extensions.terminal nemo-terminal);

      # Ermöglicht, dass Pakete Nemo-Actions unter /share/nemo/actions bereitstellen
      pathsToLink = ["/share/nemo/actions"];
    };
  };
}
