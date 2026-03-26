# modules/nixos/system/sudo/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.system.sudo;
in {
  options.${namespace}.system.sudo = with types; {
    enable = mkBoolOpt false "Enable hardened sudo defaults.";
    wheelNeedsPassword = mkBoolOpt true "Require a password for sudo by wheel users.";
    timestampTimeout = mkOpt int 5 "Minutes before sudo authentication expires.";
    passwordTries = mkOpt int 3 "Maximum number of password attempts for sudo.";
  };

  config = mkIf cfg.enable {
    security.sudo = {
      enable = true;
      execWheelOnly = true;

      extraConfig = concatStringsSep "\n" (
        [
          "Defaults env_reset"
          "Defaults use_pty"
          "Defaults passwd_tries=${toString cfg.passwordTries}"
          "Defaults timestamp_timeout=${toString cfg.timestampTimeout}"
        ]
        ++ optional (!cfg.wheelNeedsPassword) "%wheel ALL=(ALL:ALL) NOPASSWD: ALL"
      );
    };
  };
}
