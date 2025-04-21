{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.bundles.vm;
in {
  options.${namespace}.bundles.vm = with types; {
    enable = mkBoolOpt false "Whether or not to enable virtual machine configuration.";
  };

  config = mkIf cfg.enable {
    ${namespace} = {
      services = {
        ssh = enabled;
        spice-agent = enabled;
        qemu-guest = enabled;
        virtiofsd = disabled;
      };

      programs = {
        nh = enabled;
        nix-ld = enabled;
      };

      system = {
        fonts = {
          packages = with pkgs; [];
          fontconfig = disabled;
        };
      };

      hardware = {
        networking = enabled;
        bluetooth = disabled; # Für VM meist unnötig
        audio = disabled; # Wenn rein headless oder Entwicklung
      };
    };
    services.xserver.enable = lib.mkDefault false;
  };
}
