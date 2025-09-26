# modules/home/services/polkit-agent.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.polkitAgent;
in {
  options.${namespace}.services.polkitAgent = with types; {
    enable = mkBoolOpt false "Start a Polkit authentication agent in user session";
    kind = mkOpt (types.enum [
      "lxqt"
      "gnome"
    ]) "lxqt" "Agent implementation";
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf (cfg.kind == "lxqt") {services.lxqt-policykit.enable = true;})
    (mkIf (cfg.kind == "gnome") {
      systemd.user.services.polkit-gnome-agent = {
        Unit = {
          Description = "polkit-gnome authentication agent";
          After = ["graphical-session.target"];
          PartOf = ["graphical-session.target"];
        };
        Service = {
          ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
          Restart = "on-failure";
          RestartSec = 2;
        };
        Install.WantedBy = ["graphical-session.target"];
      };
    })
  ]);
}
