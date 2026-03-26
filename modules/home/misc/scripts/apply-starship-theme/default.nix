# modules/home/misc/scripts/apply-starship-theme/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.misc.scripts.apply-starship-theme;

  applyStarshipTheme = pkgs.writeShellScriptBin "apply-starship-theme" ''
        #!/usr/bin/env bash
        set -euo pipefail

        export HOME="''${HOME:-${config.home.homeDirectory}}"
        export XDG_CACHE_HOME="''${XDG_CACHE_HOME:-$HOME/.cache}"
        export XDG_CONFIG_HOME="''${XDG_CONFIG_HOME:-$HOME/.config}"

        export STATE_FILE="$XDG_CACHE_HOME/awesome/theme-state.json"
        export TEMPLATE_FILE="${toString cfg.templateFile}"
        export OUTPUT_FILE="$XDG_CONFIG_HOME/starship.toml"

        mkdir -p "$XDG_CONFIG_HOME"

        if [ ! -f "$STATE_FILE" ]; then
          echo "apply-starship-theme: state file not found: $STATE_FILE" >&2
          exit 1
        fi

        if [ ! -f "$TEMPLATE_FILE" ]; then
          echo "apply-starship-theme: template file not found: $TEMPLATE_FILE" >&2
          exit 1
        fi

        ${pkgs.python3}/bin/python3 <<'PY'
    import json
    import os
    from pathlib import Path

    state_file = Path(os.environ["STATE_FILE"])
    template_file = Path(os.environ["TEMPLATE_FILE"])
    output_file = Path(os.environ["OUTPUT_FILE"])

    data = json.loads(state_file.read_text())
    theme = data["theme"]
    palette = theme["palette"]
    roles = theme["roles"]

    def role_color(name: str) -> str:
        palette_key = roles[name]
        return palette[palette_key]

    mapping = {
        "background": role_color("background"),
        "surface": role_color("surface"),
        "surface_focus": role_color("surface_focus"),
        "text": role_color("text"),
        "text_focus": role_color("text_focus"),
        "text_invert": role_color("text_invert"),
        "text_invert_focus": role_color("text_invert_focus"),
        "primary": role_color("primary"),
        "secondary": role_color("secondary"),
        "tertiary": role_color("tertiary"),
        "start": role_color("start"),
        "start_focus": role_color("start_focus"),
        "close": role_color("close"),
        "close_focus": role_color("close_focus"),
    }

    template = template_file.read_text()
    rendered = template

    for key, value in mapping.items():
        rendered = rendered.replace("{{" + key + "}}", value)

    output_file.write_text(rendered)
    PY
  '';
in {
  options.${namespace}.misc.scripts.apply-starship-theme = with types; {
    enable = mkBoolOpt false "Enable apply-starship-theme script.";

    templateFile =
      mkOpt (types.nullOr types.path) null
      "Resolved starship template file used to generate ~/.config/starship.toml.";
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.templateFile != null;
        message = "${namespace}.misc.scripts.apply-starship-theme.templateFile must be set when enabled.";
      }
    ];

    home.packages = [
      applyStarshipTheme
      pkgs.python3
    ];
  };
}
