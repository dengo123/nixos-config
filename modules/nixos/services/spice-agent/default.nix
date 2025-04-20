{
  lib,
  config,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; {
  options.${namespace}.services.spice-agent.enable =
    mkBoolOpt false "Enable SPICE vdagent daemon.";

  config = mkIf config.${namespace}.services.spice-agent.enable {
    services.spice-vdagentd.enable = true;
  };
}
