{
  description = "marcosh/crm: compositional reproducible machines";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      # don't look for a flake.nix file in this repository
      # this tells Nix to retrieve this input as just source code
      flake = false;
    };
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      # this style makes it easy to override non-Haskell packages, e.g. to patch them
      # pkgs = import inputs.nixpkgs { inherit system; overlays = []; };
      pkgs = inputs.nixpkgs.legacyPackages.${system};

      # only consider source dirs and package.yaml as source to our Haskell package
      # this allows the project to rebuild only when source files change, not e.g. readme
      src = inputs.nix-filter.lib {
        root = ./.;
        include = import ./nix/haskell-source.nix;
      };

      # This allows us to build a Haskell package with any given GHC version.
      # It will also affects all dependent libraries.
      # overrides allows us to patch existing Haskell packages, or introduce new ones
      # see here for specifics: https://nixos.wiki/wiki/Overlays
      haskellPackagesFor =
        { ghcVersion
        , overrides ? (_: _: { })
        }:
        pkgs.haskell.packages."ghc${ghcVersion}".override {
          overrides = self: super: (overrides self super) // {
            crm = self.callCabal2nix "crm" src { };
          };
        };

      # A list of GHC versions and corresponding package overrides to use with `haskellPackagesFor`.
      configurations = import ./nix/haskell-configurations.nix { inherit inputs; };

      # A utility function that creates a set containing key-value pairs constructed for each
      # element in `configurations`.
      foldConfigurations = f:
        builtins.foldl'
          (acc: conf:
            acc // { "ghc${conf.ghcVersion}" = f (haskellPackagesFor conf); }
          )
          { }
          configurations;

      # The version of GHC used for default package and development shell.
      defaultGhcVersion = "ghc92";
    in
    rec {
      packages = {
        # Build crm for one given GHC versions.
        crm = foldConfigurations (haskellPackages: haskellPackages.crm);

        # Build crm for all GHC versions at once, collecting the results into one derivation.
        # documentation: https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/trivial-builders.nix
        crmAll = pkgs.symlinkJoin {
          name = "crmAll";
          paths = builtins.attrValues packages.crm;
        };

        default = packages.crm.${defaultGhcVersion};
      };

      # Prepare a development shell for many diffent GHC versions.
      devShells = foldConfigurations
        (haskellPackages:
          haskellPackages.shellFor {
            packages = ps: [ ps.crm ];
            nativeBuildInputs = with haskellPackages; [
              cabal-install
              fourmolu
              haskell-language-server
              hpack
            ];
            shellHook = ''
              export PS1="❄️ GHC ${haskellPackages.ghc.version} $PS1"
            '';
          }
        ) // {
        default = devShells.${defaultGhcVersion};
      };

      # The formatter to use for .nix files (but not .hs files)
      # Allows us to run `nix fmt` to reformat nix files.
      formatter = pkgs.nixpkgs-fmt;
    }
  );
}
