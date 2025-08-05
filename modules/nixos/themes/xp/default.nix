{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.themes.xp;
  winxp = import ./xfce-winxp-tc.nix {inherit pkgs;};
in {
  options.${namespace}.themes.xp = with types; {
    enable = mkBoolOpt false "Enable Windows XP Luna full theme (GTK, XFWM, Icons, Cursors, Sounds, Wallpapers, Bootscreen)";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [winxp];

    environment.pathsToLink = [
      "/share/themes"
      "/share/icons"
      "/share/sounds"
      "/share/backgrounds"
      "/share/xfce-winxp"
      "/share/fonts"
    ];
  };
}
