# modules/home/programs/starship/default.nix
{
  inputs,
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.starship;

  templateFile = inputs.self + "/dotfiles/starship/${cfg.template}.toml";
in {
  options.${namespace}.programs.starship = with types; {
    enable = mkBoolOpt false "Enable starship.";

    template =
      mkOpt types.str "powerline_wibar"
      "Starship template name from repository root /starship, without .toml suffix.";
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = builtins.pathExists templateFile;
        message = "${namespace}.programs.starship.template does not exist: ${toString templateFile}";
      }
    ];

    programs.starship.enable = true;

    ${namespace}.misc.scripts.apply-starship-theme = {
      enable = true;
      templateFile = templateFile;
    };
  };
}
