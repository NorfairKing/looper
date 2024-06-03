{ lib
, haskell
, callPackage
, ...
}:
with lib;
with haskell.lib;
self: super:
{
  looper = (buildStrictly (self.callPackage ../looper { })).overrideAttrs (old: {
    passthru = (old.passthru or { }) // {
      mkLooperOption = callPackage ./looper-option.nix { };
    };
  });
}
