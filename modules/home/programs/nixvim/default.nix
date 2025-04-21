{
  inputs,
  pkgs,
  lib,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.nixvim;
in {
  options.${namespace}.programs.nixvim = with types; {
    enable = mkBoolOpt false "Enable nixvim as primary Neovim configuration.";
  };

  imports = [
    inputs.nixvim.homeManagerModules.nixvim
  ];

  config = mkIf cfg.enable {
    programs.nixvim = {
      enable = true;
      defaultEditor = true;
      viAlias = true;
      vimAlias = true;

      extraPackages = with pkgs; [wl-clipboard];
    };

    home.file.".config/nvim/doc/nixvim.txt".source = ./nixvim.txt;
  };
}
