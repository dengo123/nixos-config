{
  config,
  lib,
  namespace,
  ...
}:
with lib;
with lib.${namespace};
let
  cfg = config.${namespace}.programs.git;
  userCfg = config.${namespace}.config.user;
in
{
  options.${namespace}.programs.git = with types; {
    enable = mkBoolOpt false "Enable Git configuration.";
    username = mkOpt (nullOr str) null "Git user name (overrides user.fullName)";
    useremail = mkOpt (nullOr str) null "Git email address (overrides user.email)";
  };

  config = mkIf cfg.enable {
    programs.git = {
      enable = true;
      lfs = enabled;
      settings = {
        user = {
          name = mkDefault (userCfg.name);
          email = mkDefault (userCfg.email);
        };
        pull.rebase = true;
        init.defaultBranch = "main";
        rebase.autoStash = true;
      };
    };
    programs.delta = {
      enable = true;
      enableGitIntegration = true;
    };
  };
}
