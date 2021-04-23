let
  sources = import ./sources.nix;
  pkgs = import ./pkgs.nix;
  nix-pre-commit-hooks = import sources.pre-commit-hooks;

in
{
  tools = with nix-pre-commit-hooks; [
    hpack
    hlint
    nixpkgs-fmt
    ormolu
  ];
  check = nix-pre-commit-hooks.run {
    src = pkgs.gitignoreSource ../.;
    hooks = {
      nixpkgs-fmt.enable = true;
      hlint.enable = true;
      ormolu.enable = true;
    };
  };
}
