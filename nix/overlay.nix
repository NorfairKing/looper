final: prev:
with final.lib;
with final.haskell.lib;
{
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = composeExtensions (old.overrides or (_: _: { })) (self: super: {
      looper = (buildStrictly (self.callPackage ../looper { })).overrideAttrs (old: {
        passthru = (old.passthru or { }) // {
          mkLooperOption = final.callPackage ./looper-option.nix { };
        };
      });
    });
  });
}
