{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.programs.git;
  userCfg = config.${namespace}.config.user;
in {
  options.${namespace}.programs.git = with types; {
    enable = mkBoolOpt false "Enable Git configuration.";
    username = mkOpt (nullOr str) null "Git user name (overrides user.fullName)";
    useremail = mkOpt (nullOr str) null "Git email address (overrides user.email)";
  };

  config = mkIf cfg.enable {
    programs.git = {
      enable = true;
      delta = enabled;
      lfs = enabled;

      userName = mkDefault (cfg.username or userCfg.fullName);
      userEmail = mkDefault (cfg.useremail or userCfg.email);

      extraConfig = {
        pull.rebase = true;
        init.defaultBranch = "main";
        rebase.autoStash = true;
      };
    };
  };
}
