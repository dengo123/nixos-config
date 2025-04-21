{ ... }:

{
  imports = [ ./extra.nix ];

  programs.nixvim.config.plugins = {
    mini-surround.enable = true;
    mini-move.enable = true;
    mini-trailspace.enable = true;
    mini-indentscope = {
      enable = true;
      settings = {
        symbol = "│";
        delay = 0;
        draw.animation = "none";
      };
    };
  };
}

