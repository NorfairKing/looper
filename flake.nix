{
  description = "looper";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-26.05";
    nixpkgs-25_11.url = "github:NixOS/nixpkgs?ref=nixos-25.11";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    sydtest.url = "github:NorfairKing/sydtest";
    sydtest.flake = false;
    opt-env-conf.url = "github:NorfairKing/opt-env-conf";
    opt-env-conf.flake = false;
    fast-myers-diff.url = "github:NorfairKing/fast-myers-diff";
    fast-myers-diff.flake = false;
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-25_11
    , pre-commit-hooks
    , autodocodec
    , safe-coloured-text
    , fast-myers-diff
    , sydtest
    , opt-env-conf
    , validity
    }:
    let
      system = "x86_64-linux";
      nixpkgsFor = nixpkgs: import nixpkgs {
        inherit system; config.allowUnfree = true;
      };
      pkgs = nixpkgsFor nixpkgs;
      haskellPackagesFor = nixpkgs: (nixpkgsFor nixpkgs).haskellPackages.extend (
        pkgs.lib.composeManyExtensions [
          (pkgs.callPackage (autodocodec + "/nix/overrides.nix") { })
          (pkgs.callPackage (safe-coloured-text + "/nix/overrides.nix") { })
          (pkgs.callPackage (fast-myers-diff + "/nix/overrides.nix") { })
          (pkgs.callPackage (sydtest + "/nix/overrides.nix") { })
          (pkgs.callPackage (opt-env-conf + "/nix/overrides.nix") { })
          (pkgs.callPackage (validity + "/nix/overrides.nix") { })
          self.overrides.${system}
        ]
      );
      haskellPackages = haskellPackagesFor nixpkgs;
    in
    {
      overrides.${system} = pkgs.callPackage ./nix/overrides.nix { };
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = haskellPackages.looper;
      checks.${system} =
        let
          backwardCompatibilityCheckFor = nixpkgs: (haskellPackagesFor nixpkgs).autodocodecRelease;
          allNixpkgs = {
            inherit
              nixpkgs-25_11;
          };
          backwardCompatibilityChecks = pkgs.lib.mapAttrs (_: nixpkgs: backwardCompatibilityCheckFor nixpkgs) allNixpkgs;
        in
        backwardCompatibilityChecks // {
          release = self.packages.${system}.default;
          pre-commit = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              hlint.enable = true;
              hpack.enable = true;
              ormolu.enable = true;
              nixpkgs-fmt.enable = true;
              nixpkgs-fmt.excludes = [ ".*/default.nix" ];
              cabal2nix.enable = true;
            };
          };
        };
      devShells.${system}.default = haskellPackages.shellFor {
        name = "looper-shell";
        packages = p: [ p.looper ];
        withHoogle = true;
        doBenchmark = true;
        buildInputs = with pkgs; [
          zlib
          cabal-install
        ] ++ self.checks.${system}.pre-commit.enabledPackages;
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
    };
}
