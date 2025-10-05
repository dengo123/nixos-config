# modules/nixos/services/greetd/default.nix
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

  # baue den Greeter-Command je nach "theme"
  greeterCmd =
    if cfg.theme == "tuigreet"
    then
      # TUI-Greeter auf der TTY; nach Login X11 mit deiner ~/.xsession starten
      ''
        ${pkgs.greetd.tuigreet}/bin/tuigreet \
                   --time --remember --asterisks \
                   --cmd "${pkgs.xorg.xinit}/bin/startx $HOME/.xsession"''
    else if cfg.theme == "gtkgreet"
    then
      # Beispiel für GTK-Greeter (X11): optional
      ''${pkgs.gtkgreet}/bin/gtkgreet -l'' # dann z.B. via wrapper startx ausführen
    else cfg.theme; # benutzerdefinierter Befehl
in {
  options.${namespace}.services.greetd = {
    enable = mkBoolOpt false "Enable greetd display manager";
    # "tuigreet" | "gtkgreet" | eigener Befehl (String)
    theme = mkOpt types.str "tuigreet" "Greeter to use (tuigreet, gtkgreet, or custom command string).";
  };

  config = mkIf cfg.enable {
    # Xorg bereitstellen (startx/xinit/Xorg)
    services.xserver.enable = true;

    # greetd einschalten
    services.greetd = {
      enable = true;
      # Auf welcher TTY (optional): z.B. 1
      # vt = 1;

      settings = {
        default_session = {
          # Wichtig: tuIGreet zeigt Login; nach erfolgreichem Login wird startx ~/.xsession ausgeführt
          command = greeterCmd;
          user = "greeter";
        };
      };
    };

    # kleines Cache-Verzeichnis für Greeter-User (optional)
    systemd.tmpfiles.rules = [
      "d /var/cache/greeter 0755 greeter greeter -"
    ];
  };
}
