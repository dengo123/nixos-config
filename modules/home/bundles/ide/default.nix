# modules/home/bundles/ide/default.nix
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
  cfg = config.${namespace}.bundles.ide;
in
{
  options.${namespace}.bundles.ide = with types; {
    enable = mkBoolOpt true "Enable IDE bundle, favorite editor with shared global developer packages";

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
        devenv

        # Essentials
        git
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
      ];

      programs.direnv = {
        enable = true;
        enableZshIntegration = true;
        nix-direnv.enable = true;
      };
    }

    # Editor-Auswahl (analog zu terminal.emulator)
    (mkIf (cfg.editor == "doom") {
      ${namespace}.programs.doom = {
        enable = true;
        doomDir = inputs.self + /dotfiles/doom;
        emacs = pkgs.emacs;
      };
      ${namespace}.bundles.shell.mode = mkOverride 900 "emacs";
      ${namespace}.bundles.terminal = mkOverride 900 disabled;
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
