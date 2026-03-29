# modules/home/programs/firefox/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.firefox;
in {
  options.${namespace}.programs.firefox = with types; {
    enable = mkBoolOpt false "Enable Firefox.";

    hardening = {
      enable = mkBoolOpt true "Enable a sensible hardened Firefox baseline.";
      resistFingerprinting = mkBoolOpt false "Enable privacy.resistFingerprinting.";
      strictTrackingProtection = mkBoolOpt true "Enable strict tracking protection.";
      disableTelemetry = mkBoolOpt true "Disable Firefox telemetry and studies.";
      disablePocket = mkBoolOpt true "Disable Firefox Pocket integration.";
      enableHttpsOnly = mkBoolOpt true "Enable HTTPS-Only mode in all windows.";
      disableFirefoxAccounts = mkBoolOpt false "Disable Firefox Sync / Firefox Accounts UI.";
    };
  };

  config = mkIf cfg.enable {
    programs.firefox = {
      enable = true;

      profiles.default = mkIf cfg.hardening.enable {
        settings = mkMerge [
          (mkIf cfg.hardening.strictTrackingProtection {
            "browser.contentblocking.category" = "strict";
            "privacy.trackingprotection.enabled" = true;
            "privacy.trackingprotection.socialtracking.enabled" = true;
            "privacy.partition.network_state" = true;
          })

          (mkIf cfg.hardening.disableTelemetry {
            "datareporting.healthreport.uploadEnabled" = false;
            "datareporting.policy.dataSubmissionEnabled" = false;
            "toolkit.telemetry.enabled" = false;
            "toolkit.telemetry.unified" = false;
            "app.shield.optoutstudies.enabled" = false;
            "browser.discovery.enabled" = false;
          })

          (mkIf cfg.hardening.disablePocket {
            "extensions.pocket.enabled" = false;
          })

          (mkIf cfg.hardening.enableHttpsOnly {
            "dom.security.https_only_mode" = true;
            "dom.security.https_only_mode_ever_enabled" = true;
          })

          (mkIf cfg.hardening.disableFirefoxAccounts {
            "identity.fxaccounts.enabled" = false;
          })

          (mkIf cfg.hardening.resistFingerprinting {
            "privacy.resistFingerprinting" = true;
          })
        ];
      };
    };

    home.sessionVariables.BROWSER = "firefox";
  };
}
