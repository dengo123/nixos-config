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
  pkgLXQT = pkgs.lxqt-policykit; # bin: lxqt-policykit
  pkgGNOME = pkgs.polkit_gnome; # bin: polkit-gnome-authentication-agent-1
in {
  options.${namespace}.services.polkit-agent = with types; {
    enable = mkBoolOpt false "Start a Polkit authentication agent in user session.";
    kind = mkOpt (types.enum [
      "lxqt"
      "gnome"
    ]) "lxqt" "Which agent to run.";
  };

  config = mkIf cfg.enable {
    # LXQt Agent
    systemd.user.services.polkit-lxqt-agent = mkIf (cfg.kind == "lxqt") {
      Unit = {
        Description = "LXQt PolicyKit authentication agent";
        After = ["graphical-session.target"];
        PartOf = ["graphical-session.target"];
      };
      Service = {
        ExecStart = "${pkgLXQT}/bin/lxqt-policykit";
        Restart = "on-failure";
        RestartSec = 2;
      };
      Install.WantedBy = ["graphical-session.target"];
    };

    # GNOME Agent
    systemd.user.services.polkit-gnome-agent = mkIf (cfg.kind == "gnome") {
      Unit = {
        Description = "GNOME PolicyKit authentication agent";
        After = ["graphical-session.target"];
        PartOf = ["graphical-session.target"];
      };
      Service = {
        ExecStart = "${pkgGNOME}/libexec/polkit-gnome-authentication-agent-1";
        Restart = "on-failure";
        RestartSec = 2;
      };
      Install.WantedBy = ["graphical-session.target"];
    };

    home.packages = [
      (
        if cfg.kind == "lxqt"
        then pkgLXQT
        else pkgGNOME
      )
    ];
  };
}
