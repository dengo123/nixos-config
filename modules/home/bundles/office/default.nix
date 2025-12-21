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
      kdePackages.okular

      hunspell
      hunspellDicts.de_DE
      hunspellDicts.en_US

      planify
      newsflash
      thunderbird
      pandoc
    ];
  };
}
