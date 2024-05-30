{ lib
, haskell
, ...
}:
with lib;
with haskell.lib;
self: super:
{
  looper = (buildStrictly (self.callPackage ../looper { })).overrideAttrs (old: {
    passthru = (old.passthru or { }) // {
      mkLooperOption = final.callPackage ./looper-option.nix { };
    };
  });
}
