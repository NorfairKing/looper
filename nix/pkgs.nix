let
  sources = import ./sources.nix;
  pkgsf = import sources.nixpkgs;
in
pkgsf {
  overlays = [
    (import (sources.safe-coloured-text + "/nix/overlay.nix"))
    (import (sources.yamlparse-applicative + "/nix/overlay.nix"))
    (import (sources.sydtest + "/nix/overlay.nix"))
    (final: previous: { inherit (import sources.gitignore { inherit (final) lib; }) gitignoreSource; })
    (import ./overlay.nix)
  ];
}
