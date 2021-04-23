final: previous: {
  haskellPackages = previous.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (self: super: {
      looper = final.haskell.lib.buildStrictly
        (self.callCabal2nixWithOptions "looper" (final.gitignoreSource ../looper) "--no-hpack" { });
    });
  });
}
