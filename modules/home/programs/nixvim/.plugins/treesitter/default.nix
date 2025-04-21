{ ... }:

{
  imports = [
    ./plugins.nix
    ./extra.nix
  ];

  programs.nixvim = {
    config = {
      enable = true;

      plugins = {
        treesitter = {
          enable = true;

          settings = {
            ensure_installed = "all";
            highlight.enable = true;
            indent.enable = true;
          };
        };

        rainbow-delimiters.enable = true;
        treesitter-context.enable = true;
      };
    };
  };
}

