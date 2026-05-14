# modules/home/bundles/developer/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.bundles.developer;
in {
  options.${namespace}.bundles.developer = with types; {
    enable = mkBoolOpt true "Enable IDE bundle, favorite editor with shared global developer packages";

    editor =
      mkOpt (types.nullOr (
        types.enum [
          "doom"
          # "nvim"
        ]
      ))
      null "Choose a text editor. If null, only vanilla vim is installed (no dev stack).";
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf (cfg.editor == null) {
      home.packages = with pkgs; [
        vim
        micro
        git
      ];
    })

    (mkIf (cfg.editor != null) {
      home.packages = with pkgs; [
        clang-tools
        cmake
        devenv

        # Essentials
        codex
        coreutils
        editorconfig-core-c
        fd
        findutils
        gcc
        gnumake
        gnugrep
        gnutar
        gzip
        just
        libtool
        ripgrep
        shellcheck
        sqlite

        # Writing / spellcheck
        pkg-config
        enchant
        (lib.getDev pkgs.enchant)
        hunspell
        hunspellDicts.de_DE
        hunspellDicts.en_US

        # Lua
        lua-language-server
        stylua

        # Nix
        alejandra
        nixd
        nixfmt-rfc-style

        # Python
        black
        isort
        pipenv
        python3Packages.pyflakes
        pyright
        python3Packages.pytest
        python3
        ruff

        # JSON / TOML / YAML / XML
        jq
        libxml2
        taplo
        vscode-langservers-extracted
        yamlfmt
        yaml-language-server

        # Shell / web
        js-beautify
        stylelint
        shfmt
        html-tidy

        # JS / TS
        eslint
        prettier
        nodejs_22
        typescript
        typescript-language-server
      ];

      ${namespace}.programs = {
        git = enabled;

        aider-chat = mkDefault enabled;
      };
      programs.direnv = {
        enable = true;
        enableZshIntegration = true;
        nix-direnv.enable = true;
      };
    })

    (mkIf (cfg.editor == "doom") {
      ${namespace} = {
        programs.doom.enable = true;
        bundles.shell.mode = mkDefault "emacs";
      };
    })

    # (mkIf (cfg.editor == "nvim") {
    #   ${namespace}.programs.nixvim = {
    #     enable = true;
    #     mode = mkDefault "full";
    #   };
    # })
  ]);
}
