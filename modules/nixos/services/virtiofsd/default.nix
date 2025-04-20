{
  lib,
  config,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; {
  options.${namespace}.services.virtiofsd.enable =
    mkBoolOpt false "Enable virtiofsd (shared folders via virtio-fs).";

  config = mkIf config.${namespace}.services.virtiofsd.enable {
    systemd.services.virtiofsd = {
      description = "VirtioFS Daemon";
      wantedBy = ["multi-user.target"];
      after = ["network.target"];
      serviceConfig = {
        ExecStart = "${pkgs.virtiofsd}/bin/virtiofsd --socket-path=/dev/shm/virtiofsd.sock --shared-dir=/mnt/hostshare";
        Restart = "on-failure";
      };
    };

    fileSystems."/mnt/hostshare" = {
      device = "hostshare";
      fsType = "virtiofs";
      options = ["defaults"];
    };
  };
}
