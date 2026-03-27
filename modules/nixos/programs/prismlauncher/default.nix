# modules/nixos/programs/prismlauncher/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.prismlauncher;
in {
  options.${namespace}.programs.prismlauncher = with types; {
    enable = mkBoolOpt false "Enable Prism Launcher.";
    desktopName = mkOpt str "Minecraft" "Desktop entry name for Prism Launcher.";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [pkgs.prismlauncher];

    environment.etc."xdg/applications/minecraft.desktop".text = ''
      [Desktop Entry]
      Version=1.0
      Type=Application
      Name=${cfg.desktopName}
      Comment=Minecraft via Prism Launcher
      Exec=${pkgs.prismlauncher}/bin/prismlauncher
      Icon=prismlauncher
      Categories=Game;
      StartupNotify=true
      Terminal=false
    '';
  };
}
