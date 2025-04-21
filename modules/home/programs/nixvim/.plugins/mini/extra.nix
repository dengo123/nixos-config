{ lib, pkgs, ... }:

{
  programs.nixvim = {
    config = {
      extraPlugins = with pkgs.vimPlugins; [
        mini-ai
        mini-jump
        mini-bracketed
        mini-comment
      ];

      extraConfigLua = lib.mkAfter ''
        require("mini.ai").setup()
        require("mini.jump").setup()
        require("mini.bracketed").setup()
        require("mini.comment").setup()
      '';
    };
  };
}

