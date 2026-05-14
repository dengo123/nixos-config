# modules/nixos/services/wireplumber/default.nix
{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.services.wireplumber;

  mkRule = nodeName: priority: {
    matches = [
      {
        "node.name" = nodeName;
      }
    ];

    actions = {
      "update-props" = {
        "priority.session" = priority;
      };
    };
  };

  alsaRules =
    (optional (cfg.sinks.headset.alsaNodeName != null)
      (mkRule cfg.sinks.headset.alsaNodeName cfg.sinks.headset.priority))
    ++ (optional (cfg.sinks.tv.alsaNodeName != null)
      (mkRule cfg.sinks.tv.alsaNodeName cfg.sinks.tv.priority))
    ++ (optional (cfg.sinks.speakers.alsaNodeName != null)
      (mkRule cfg.sinks.speakers.alsaNodeName cfg.sinks.speakers.priority));

  bluezRules =
    (optional (cfg.sinks.headset.bluezNodeName != null)
      (mkRule cfg.sinks.headset.bluezNodeName cfg.sinks.headset.priority))
    ++ (optional (cfg.sinks.tv.bluezNodeName != null)
      (mkRule cfg.sinks.tv.bluezNodeName cfg.sinks.tv.priority))
    ++ (optional (cfg.sinks.speakers.bluezNodeName != null)
      (mkRule cfg.sinks.speakers.bluezNodeName cfg.sinks.speakers.priority));
in {
  options.${namespace}.services.wireplumber = with types; {
    enable = mkBoolOpt false "Whether or not to enable custom WirePlumber policy.";

    restoreDefaultTargets = mkBoolOpt false ''
      Whether or not WirePlumber should restore previously remembered default sinks.
      Disable this to always prefer the highest-priority available sink.
    '';

    sinks = {
      headset = {
        priority = mkOpt int 1400 "Priority for the preferred headset sink.";
        alsaNodeName = mkOpt (nullOr str) null "Regex or exact ALSA node.name for the headset sink.";
        bluezNodeName = mkOpt (nullOr str) null "Regex or exact BlueZ node.name for the headset sink.";
      };

      tv = {
        priority = mkOpt int 1300 "Priority for the TV sink.";
        alsaNodeName = mkOpt (nullOr str) null "Regex or exact ALSA node.name for the TV sink.";
        bluezNodeName = mkOpt (nullOr str) null "Regex or exact BlueZ node.name for the TV sink.";
      };

      speakers = {
        priority = mkOpt int 1200 "Priority for the normal speaker sink.";
        alsaNodeName = mkOpt (nullOr str) null "Regex or exact ALSA node.name for the speaker sink.";
        bluezNodeName = mkOpt (nullOr str) null "Regex or exact BlueZ node.name for the speaker sink.";
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = config.services.pipewire.enable or false;
        message = "PipeWire must be enabled for ${namespace}.services.wireplumber.";
      }
      {
        assertion = config.services.pipewire.wireplumber.enable or false;
        message = "services.pipewire.wireplumber.enable must be true for ${namespace}.services.wireplumber.";
      }
    ];

    services.pipewire.wireplumber.extraConfig."51-${namespace}-sink-priority" = {
      "wireplumber.settings" = {
        "node.restore-default-targets" = cfg.restoreDefaultTargets;
      };

      "monitor.alsa.rules" = alsaRules;
      "monitor.bluez.rules" = bluezRules;
    };
  };
}
