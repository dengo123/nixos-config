{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.greetd;
in {
  options.${namespace}.services.greetd = {
    enable = mkBoolOpt false "Enable greetd display manager";
    theme = mkOpt types.str "tuigreet" "Greeter theme to use (e.g. tuigreet, gtkgreet, ags, custom)";
  };

  config = mkIf cfg.enable {
    services.greetd = {
      enable = true;
      settings.default_session.command = pkgs.writeShellScript "greeter" ''
        export XKB_DEFAULT_LAYOUT=${config.services.xserver.xkb.layout or "us"}
        export XCURSOR_THEME="Qogir"

        ${cfg.theme}
      '';
    };

    systemd.tmpfiles.rules = [
      "d /var/cache/greeter 0755 greeter greeter -"
    ];
  };
}
