# modules/nixos/system/filesystem/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.system.filesystem;

  tmpOptions =
    ["mode=1777"]
    ++ optional cfg.tmp.nodev "nodev"
    ++ optional cfg.tmp.nosuid "nosuid"
    ++ optional cfg.tmp.noexec "noexec";
in {
  options.${namespace}.system.filesystem = with types; {
    enable = mkBoolOpt false "Enable hardened filesystem defaults.";

    tmp = {
      useTmpfs = mkBoolOpt true "Mount /tmp as tmpfs.";
      tmpfsSize = mkOpt str "2G" "Size limit for /tmp tmpfs.";
      nodev = mkBoolOpt true "Mount /tmp with nodev.";
      nosuid = mkBoolOpt true "Mount /tmp with nosuid.";
      noexec = mkBoolOpt false "Mount /tmp with noexec.";
      cleanOnBoot = mkBoolOpt true "Clean /tmp on boot.";
    };
  };

  config = mkIf cfg.enable {
    boot.tmp = {
      useTmpfs = cfg.tmp.useTmpfs;
      tmpfsSize = cfg.tmp.tmpfsSize;
      cleanOnBoot = cfg.tmp.cleanOnBoot;
    };
  };
}
