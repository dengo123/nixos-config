{
  options,
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.namespace;
let
  cfg = config.${namespace}.bundles.ai;
in
{
  options.${namespace}.bundles.ai = with types; {
    enable = mkBoolOpt false "Enable bundles.ai";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      whisper-cpp
    ];

    ${namespace} = {
      hardware.cuda = enabled;
      services.ollama = {
        enable = true;
        packages = pkgs.ollama-cuda;
      };
    };
  };
}
