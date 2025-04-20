{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.ssh;
in {
  options.${namespace}.services.ssh = with types; {
    enable = mkBoolOpt false "Enable OpenSSH server";
    passwordAuth = mkBoolOpt false "Enable password authentication (not recommended)";
    permitRootLogin = mkStrOpt "no" "Permit root login (\"yes\", \"no\", \"prohibit-password\")";
    openFirewall = mkBoolOpt true "Open port 22 in the firewall";
    packages = mkOpt (listOf package) [pkgs.sshs] "Additional packages related to SSH";
  };

  config = mkIf cfg.enable {
    services.openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = cfg.passwordAuth;
        PermitRootLogin = cfg.permitRootLogin;
      };
    };

    environment.systemPackages = cfg.packages;

    networking.firewall.allowedTCPPorts = mkIf cfg.openFirewall [22];
  };
}
