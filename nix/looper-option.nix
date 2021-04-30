{ lib, ... }:
with lib;
name: mkOption {
  default = null;
  type = types.nullOr (types.submodule {
    options = {
      enable = mkEnableOption "${name} looper";
      phase = mkOption {
        type = types.nullOr types.int;
        default = null;
        example = 60;
      };
      period = mkOption {
        type = types.nullOr types.int;
        default = null;
        example = 0;
      };
    };
  });
}
