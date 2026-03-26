# modules/home/services/polkit-agent/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.polkit-agent;
  pkg = pkgs.polkit_gnome;
in {
  options.${namespace}.services.polkit-agent = with types; {
    enable = mkBoolOpt false "Start the GNOME polkit authentication agent in the user session.";
  };

  config = mkIf cfg.enable {
    systemd.user.services.polkit-gnome-agent = {
      Unit = {
        Description = "GNOME PolicyKit authentication agent";
        After = ["graphical-session.target"];
        PartOf = ["graphical-session.target"];
      };

      Service = {
        ExecStart = "${pkg}/libexec/polkit-gnome-authentication-agent-1";
        Restart = "on-failure";
        RestartSec = 2;
      };

      Install = {
        WantedBy = ["graphical-session.target"];
      };
    };

    home.packages = [pkg];
  };
}
