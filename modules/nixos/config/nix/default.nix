# module/nixos/config/nix/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace};
let
  cfg = config.${namespace}.config.nix;
  user = config.${namespace}.config.user;
in
{
  options.${namespace}.config.nix = {
    enable = mkBoolOpt false "Enable nix configuration";

    # === System-Generationen automatisch rotieren ============================
    rotateGenerations = {
      enable = mkBoolOpt true "Delete old *system* profile generations on a schedule.";
      olderThan =
        mkOpt types.str "30d"
          "Age selector passed to `nix-env --delete-generations` (e.g. '30d', '10', '1m').";
      schedule =
        mkOpt types.str "weekly"
          "Systemd OnCalendar expression (e.g. 'weekly', 'daily', 'Mon *-*-* 03:00:00').";
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      nix-health
      nix-index
      nix-output-monitor
      nix-prefetch-git
      nixfmt-rfc-style
    ];

    nix =
      let
        users = [
          "root"
          user.name
        ];
      in
      {
        # package = pkgs.nixVersions.latest;
        gc = {
          options = "--delete-older-than 30d";
          dates = "daily";
          automatic = true;
        };
        settings = {
          trusted-users = users;
          sandbox = true;
          sandbox-fallback = false;
          require-sigs = true;
          auto-optimise-store = true;
          allowed-users = users;
          experimental-features = "nix-command flakes";
          http-connections = 50;
          warn-dirty = false;
          log-lines = 50;
        };
        generateRegistryFromInputs = true;
        generateNixPathFromInputs = true;
        linkInputs = true;
      };

    # === Systemd: System-Generationen rotieren ===============================
    systemd.services.rotate-system-generations = mkIf cfg.rotateGenerations.enable {
      description = "Prune old *system* profile generations";
      serviceConfig = {
        Type = "oneshot";
        # Benutze die tatsächlich konfigurierte Nix-Version:
        ExecStart = "${config.nix.package}/bin/nix-env --profile /nix/var/nix/profiles/system --delete-generations ${cfg.rotateGenerations.olderThan}";
      };
    };

    systemd.timers.rotate-system-generations = mkIf cfg.rotateGenerations.enable {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = cfg.rotateGenerations.schedule;
        Persistent = true;
      };
    };
  };
}
