{ inputs, system }:

#with pkgs.haskell.lib;
[
  {
    ghcVersion = "88";
    haskellPackages = inputs.nixpkgs-stable.legacyPackages.${system}.haskellPackages;
  }
  {
    ghcVersion = "90";
    haskellPackages = inputs.nixpkgs-stable.legacyPackages.${system}.haskellPackages;
  }
  { ghcVersion = "92"; }
  { ghcVersion = "94"; }
]
