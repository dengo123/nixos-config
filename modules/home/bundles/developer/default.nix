# modules/home/bundles/developer/default.nix
{
  inputs,
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace};
let
  cfg = config.${namespace}.bundles.developer;
in
{
  options.${namespace}.bundles.developer = with types; {
    enable = mkBoolOpt true "Enable developer bundle, favorite editor with shared global developer packages";

    editor = mkOpt (types.nullOr (
      types.enum [
        "doom"
        "nvim"
      ]
    )) null "Choose a text editor. If null, vanilla vim is installed as fallback.";
  };

  config = mkIf cfg.enable (mkMerge [
    # Gemeinsame Dev-Pakete
    {
      home.packages = with pkgs; [
        clang-tools
        cmake

        # Essentials
        ripgrep
        fd
        just

        # Lua
        lua-language-server
        stylua

        # Nix
        nixd
        alejandra

        # Python
        python3
        pyright
        black
        ruff

        # JSON/TOML/YAML
        vscode-langservers-extracted
        yaml-language-server
        taplo
        jq
        yamlfmt

        typescript-language-server
        prettier
      ];

      programs.direnv = {
        enable = true;
        enableZshIntegration = true;
        nix-direnv.enable = true;
      };
      ${namespace} = {
        programs.git = enabled;
      }
    }

    # Editor-Auswahl (analog zu terminal.emulator)
    (mkIf (cfg.editor == "doom") {
      ${namespace} = {
        programs.doom = {
          enable = true;
          doomDir = inputs.self + /dotfiles/doom;
          emacs = pkgs.emacs;
        };

        bundles.shell.mode = mkOverride 900 "emacs";

        bundles.terminal.enable = mkOverride 900 false;
      };
    })

    (mkIf (cfg.editor == "nvim") {
      ${namespace}.programs.nixvim = {
        enable = true;
        mode = mkDefault "full";
      };
    })

    (mkIf (cfg.editor == null) {
      home.packages = [ pkgs.vim ];
    })
  ]);
}
