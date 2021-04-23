let
  pre-commit = import ./nix/pre-commit.nix;
  pkgs = import ./nix/pkgs.nix;

in
{
  "pre-commit-check" = pre-commit.check;
  "looper" = pkgs.haskellPackages.looper;
}
