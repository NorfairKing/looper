{ lib }:
with lib;
name: mkOption {
  default = null;
  description = mdDoc "The ${name} looper";
  type = types.nullOr (types.submodule {
    options = {
      enable = mkEnableOption (mdDoc "${name} looper");
      phase = mkOption {
        type = types.nullOr types.int;
        default = null;
        example = 60;
        description = mdDoc "How long to wait before the first activation of the ${name} looper.";
      };
      period = mkOption {
        type = types.nullOr types.int;
        default = null;
        example = 0;
        description = mdDoc "How long to wait after each activation of the ${name} looper.";
      };
    };
  });
}
