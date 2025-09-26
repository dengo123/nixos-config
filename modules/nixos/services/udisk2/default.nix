{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.udisks2;
in {
  options.${namespace}.services.udisks2 = with types; {
    enable = mkBoolOpt false "Enable udisks2 (storage backend for udiskie)";
  };

  config = mkIf cfg.enable {
    services.udisks2.enable = true;
    security.polkit.enable = true; # falls security.nix nicht aktiv ist
  };
}
