{
  description = "looper";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-22.11";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    autodocodec.url = "github:NorfairKing/autodocodec?ref=flake";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text?ref=flake";
    safe-coloured-text.flake = false;
    sydtest.url = "github:NorfairKing/sydtest?ref=flake";
    sydtest.flake = false;
    validity.url = "github:NorfairKing/validity?ref=flake";
    validity.flake = false;
    nixpkgs-22_05.url = "github:NixOS/nixpkgs?ref=nixos-22.05";
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-22_05
    , flake-utils
    , pre-commit-hooks
    , autodocodec
    , safe-coloured-text
    , sydtest
    , validity
    }:
    let
      system = "x86_64-linux";
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.${system}
          (import (autodocodec + "/nix/overlay.nix"))
          (import (safe-coloured-text + "/nix/overlay.nix"))
          (import (sydtest + "/nix/overlay.nix"))
          (import (validity + "/nix/overlay.nix"))
        ];
      };
      pkgs = pkgsFor nixpkgs;
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = pkgs.haskellPackages.looper;
      checks.${system} =
        let
          backwardCompatibilityCheckFor = nixpkgs:
            let pkgs' = pkgsFor nixpkgs;
            in pkgs'.haskellPackages.looper;
          allNixpkgs = {
            inherit
              nixpkgs-22_05;
          };
          backwardCompatibilityChecks = pkgs.lib.mapAttrs (_: nixpkgs: backwardCompatibilityCheckFor nixpkgs) allNixpkgs;
        in
        backwardCompatibilityChecks // {
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
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "looper-shell";
        packages = (p:
          [ p.looper ]
        );
        withHoogle = true;
        doBenchmark = true;
        buildInputs = with pkgs; [
          niv
          zlib
          cabal-install
        ] ++ (with pre-commit-hooks;
          [
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            cabal2nix
          ]);
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
    };
}
