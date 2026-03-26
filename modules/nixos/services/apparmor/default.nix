# modules/nixos/services/apparmor/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.apparmor;
in {
  options.${namespace}.services.apparmor = with types; {
    enable = mkBoolOpt false "Enable AppArmor MAC framework.";

    enableProfiles =
      mkBoolOpt true
      "Load default AppArmor profiles.";

    enforce =
      mkBoolOpt false
      "Enforce profiles instead of complain mode.";
  };

  config = mkIf cfg.enable {
    security.apparmor = {
      enable = true;

      packages =
        optional cfg.enableProfiles pkgs.apparmor-profiles;

      # complain = log only
      # enforce = block
      killUnconfinedConfinables = false;
    };
  };
}
