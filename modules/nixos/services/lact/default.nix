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
  cfg = config.${namespace}.services.lact;
in
{
  options.${namespace}.services.lact = with types; {
    enable = mkBoolOpt false "Enable LACT (GPU control) daemon + tooling.";

    desktopEntry = mkBoolOpt true "Install a .desktop entry so LACT appears in launchers/menus.";

    # optional: for future - pick a default profile name you want to auto-apply
    # defaultProfile = mkOpt (types.nullOr types.str) null "Default LACT profile to auto-apply (if supported by your workflow).";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      services.lact.enable = true;
      environment.systemPackages = [ pkgs.lact ];

      environment.etc."xdg/applications/lact.desktop".text = ''
        [Desktop Entry]
        Name=LACT
        Comment=Linux GPU Control Tool
        Exec=${pkgs.lact}/bin/lact
        Terminal=false
        Type=Application
        Categories=System;Settings;
      '';
    }
  ]);
}
