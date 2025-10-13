{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.bundles.development;
in {
  options.${namespace}.bundles.development = with types; {
    enable = mkBoolOpt false "Enable development bundle";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      clang-tools
      cmake
      python3
      devenv
      # Essentials
      git
      ripgrep
      fd
      just
      # Lua
      lua-language-server
      stylua
      luacheck
      # Nix
      nixd
      alejandra
      # Python
      pyright
      black
      ruff
      # JSON/TOML/YAML
      vscode-langservers-extracted
      yaml-language-server
      taplo
      jq
      yamlfmt
    ];

    programs.direnv = {
      enable = true;
      enableZshIntegration = true; # see note on other shells below
      nix-direnv.enable = true;
    };
  };
}
