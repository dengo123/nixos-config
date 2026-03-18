# modules/home/services/picom/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.picom;
in {
  options.${namespace}.services.picom = with types; {
    enable = mkBoolOpt false "Enable Picom compositor.";

    package = mkOpt package pkgs.picom "Picom package to use.";

    backend = mkOpt str "glx" "Picom backend.";
    vSync = mkOpt bool true "Enable VSync.";
    fade = mkOpt bool false "Enable fade effects.";
    shadow = mkOpt bool false "Enable shadows.";

    activeOpacity = mkOpt float 1.0 "Opacity for active windows.";
    inactiveOpacity = mkOpt float 1.0 "Opacity for inactive windows.";

    cornerRadius = mkOpt int 0 "Global corner radius.";
    roundBorders = mkOpt int 0 "Rounded border radius.";

    useDefaultWintypes = mkOpt bool true "Apply default wintype rules for panels and menus.";

    extraSettings = mkOpt attrs {} "Additional raw settings merged into services.picom.settings.";
  };

  config = mkIf cfg.enable {
    services.picom = {
      enable = true;
      package = cfg.package;

      backend = cfg.backend;
      vSync = cfg.vSync;

      shadow = cfg.shadow;
      fade = cfg.fade;

      activeOpacity = cfg.activeOpacity;
      inactiveOpacity = cfg.inactiveOpacity;

      settings =
        {
          corner-radius = cfg.cornerRadius;
          round-borders = cfg.roundBorders;
        }
        // optionalAttrs cfg.useDefaultWintypes {
          wintypes = {
            dock = {
              shadow = cfg.shadow;
              clip-shadow-above = true;
            };

            tooltip = {
              shadow = cfg.shadow;
            };

            dropdown_menu = {
              shadow = cfg.shadow;
            };

            popup_menu = {
              shadow = cfg.shadow;
            };
          };
        }
        // cfg.extraSettings;
    };
  };
}
