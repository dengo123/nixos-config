{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.system.region;
in {
  options.${namespace}.system.region = {
    locale = mkOpt types.str "en_US.UTF-8" "The system locale string.";
    timeZone = mkOpt types.str "UTC" "The system timezone string.";
  };

  config = {
    i18n.defaultLocale = cfg.locale;
    i18n.supportedLocales = [cfg.locale];
    time.timeZone = cfg.timeZone;
  };
}
