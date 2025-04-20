{
  options,
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.virtualisation;
in {
  options.${namespace}.services.virtualisation = with types; {
    enable = mkBoolOpt false "Enable virtualization";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      # GUI Tools
      virt-manager
      virt-viewer
      spice-gtk
      gtk-vnc
      virt-top

      # Core Virtualisation Tools
      qemu_kvm
      libvirt
      win-virtio

      # CLI Helpers
      dmidecode
      pciutils
      usbutils

      # Optional Distrobox tools
      boxbuddy
      distrobox
      bottles
      docker-compose
    ];

    services.udev.packages = [pkgs.libvirt];

    virtualisation = {
      libvirtd = {
        enable = true;
        qemu = {
          package = pkgs.qemu_kvm;
          runAsRoot = false;
        };
      };
      docker.enable = true;
      spiceUSBRedirection.enable = true;
    };

    programs.virt-manager.enable = true;
    programs.localsend.enable = true;

    systemd.services.libvirtd = {
      wantedBy = ["multi-user.target"];
      restartIfChanged = mkForce true;
      restartTriggers = [config.virtualisation.libvirtd.qemu.package];
    };

    systemd.sockets.libvirtd.enable = true;
    systemd.services."libvirt-guests".enable = false;
  };
}
