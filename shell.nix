let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix;
  pre-commit = import ./nix/pre-commit.nix;

in
pkgs.haskell.lib.buildStackProject {
  name = "looper-shell";
  buildInputs = with pkgs; [
    (import sources.niv { }).niv
  ] ++ pre-commit.tools;
  shellHook = ''
    export TMPDIR=/tmp
    ${pre-commit.check.shellHook}
  '';
}
