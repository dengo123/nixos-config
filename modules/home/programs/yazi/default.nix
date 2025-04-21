{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.yazi;
in {
  options.${namespace}.programs.yazi = with types; {
    enable = mkBoolOpt false "Enable Yazi file manager";
  };

  config = mkIf cfg.enable {
    programs.yazi.enable = true;

    home.file.".config/yazi/yazi.toml".source = ./yazi.toml;
    home.file.".config/yazi/keymap.toml".source = ./keymap.toml;

    programs.zsh.initExtra = ''
      function y() {
        local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
        yazi "$@" --cwd-file="$tmp"
        if cwd="$(cat "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
          cd -- "$cwd"
        fi
        rm -f -- "$tmp"
      }
    '';
  };
}
