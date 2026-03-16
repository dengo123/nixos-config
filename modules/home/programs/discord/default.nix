# modules/home/programs/discor/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.discord;
in {
  options.${namespace}.programs.discord = with types; {
    enable = mkBoolOpt false "Enable Discord - All-in-one cross-platform voice and text chat for gamers";
  };

  config = mkIf cfg.enable {
    programs.discord = {
      enable = true;
      settings.SKIP_HOST_UPDATE = true;
    };
  };
}
