{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace};
let
  cfg = config.${namespace}.bundles.office;
in
{
  options.${namespace}.bundles.office = with types; {
    enable = mkBoolOpt false "Enable office bundle";
  };

  config = mkIf cfg.enable {
    nixforge.programs.freeoffice = enabled;
    home.packages = with pkgs; [
      calibre
      obsidian
      kdePackages.okular

      planify
      newsflash
      thunderbird
      pandoc
    ];
  };
}
