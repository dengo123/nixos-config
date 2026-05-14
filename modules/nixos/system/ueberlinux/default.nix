# modules/nixos/system/ueberlinux/default.nix
{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.system.ueberlinux;

  defaultManifest = [
    ""
    "→ Thus spoke the System:"
    ""
    "  I am not a distribution."
    "  I am the will to declarativity made manifest."
    ""
    "  Where others break, I roll back."
    "  Where others guess, I configure."
    "  I do not ask: 'What is installed?'"
    "  I declare: 'This shall be so.'"
    ""
    "  Behold, for I am Ueber-Linux -"
    "  Risen above the aptitudes of apt,"
    "  Freed from the desires of yum."
    "  I am NixOS: eternal, reproducible, sublime."
    ""
  ];

  manifestText = concatStringsSep "\n" cfg.manifest;

  manifestScript = pkgs.writeShellScript "ueberlinux-manifest" ''
    set -eu

    tty_path="/dev/${cfg.tty}"

    if [ ! -e "$tty_path" ]; then
      exit 0
    fi

    ${optionalString cfg.clearScreen ''
      ${pkgs.coreutils}/bin/printf '\033c' > "$tty_path"
    ''}

    ${pkgs.coreutils}/bin/cat <<'EOF' > "$tty_path"
    ${manifestText}
    EOF

    ${optionalString (cfg.pauseSeconds > 0) ''
      ${pkgs.coreutils}/bin/sleep ${toString cfg.pauseSeconds}
    ''}

    ${optionalString cfg.clearAfterDisplay ''
      ${pkgs.coreutils}/bin/printf '\033c' > "$tty_path"
    ''}
  '';
in {
  options.${namespace}.system.ueberlinux = with types; {
    enable = mkBoolOpt false "Whether or not to enable the Ueber-Linux manifest.";

    tty = mkOpt str "tty1" "TTY device that should receive the manifest output.";

    clearScreen = mkBoolOpt true "Whether or not to clear the target TTY before printing the manifest.";

    clearAfterDisplay = mkBoolOpt true "Whether or not to clear the target TTY after showing the manifest.";

    pauseSeconds = mkOpt int 21 "How long the manifest should remain visible after printing.";

    showOnBoot = mkBoolOpt true "Whether or not to show the manifest during boot before the display manager starts.";

    manifest = mkOpt (listOf str) defaultManifest "Lines to print for the Ueber-Linux manifest.";
  };

  config = mkIf cfg.enable {
    systemd.services.ueberlinux-manifest = mkIf cfg.showOnBoot {
      description = "Ueber-Linux boot manifest";
      wantedBy = ["multi-user.target"];
      before = [
        "getty@tty1.service"
        "display-manager.service"
      ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = manifestScript;
      };
    };
  };
}
