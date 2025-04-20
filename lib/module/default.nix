{lib, ...}:
with lib; rec {
  ## === UNIVERSAL OPTION HELPERS ===

  ## Create a general module option.
  ## Example: mkOpt types.str "default" "description"
  mkOpt = type: default: description:
    mkOption {inherit type default description;};

  ## Same as mkOpt, but without description
  mkOpt' = type: default:
    mkOpt type default null;

  ## === BOOL ===
  mkBoolOpt = mkOpt types.bool;
  mkBoolOpt' = mkOpt' types.bool;

  ## === STRING ===
  mkStrOpt = mkOpt types.str;
  mkStrOpt' = mkOpt' types.str;

  ## === INT ===
  mkIntOpt = mkOpt types.int;
  mkIntOpt' = mkOpt' types.int;

  ## === ENUM ===
  mkEnumOpt = values: default: description:
    mkOpt (types.enum values) default description;

  mkEnumOpt' = values: default:
    mkEnumOpt values default null;

  ## === LIST OF STRINGS ===
  mkStrListOpt = mkOpt (types.listOf types.str);
  mkStrListOpt' = mkOpt' (types.listOf types.str);

  ## === LIST OF PACKAGES ===
  mkPkgListOpt = mkOpt (types.listOf types.package);
  mkPkgListOpt' = mkOpt' (types.listOf types.package);

  ## === PREDEFINED SHORTCUTS ===

  enabled = {
    enable = true;
  };

  disabled = {
    enable = false;
  };
}
