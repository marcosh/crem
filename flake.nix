{
  description = "tweag/cerm: compositional reproducible executable machines";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-22.11";
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
        , haskellPackages ? pkgs.haskell.packages."ghc${ghcVersion}"
        }:
        haskellPackages.override {
          overrides = self: super: {
            hpack = pkgs.hpack;
            crem = (self.callCabal2nix "crem" src { }).overrideAttrs (attrs: {
              # doctest-parallel needs to know where the compiled crem package is
              preCheck = ''
                export GHC_PACKAGE_PATH="dist/package.conf.inplace:$GHC_PACKAGE_PATH"
              '';
            });
            fourmolu = pkgs.haskell.packages.ghc944.fourmolu;
          };
        };

      # A list of GHC versions and corresponding package overrides to use with `haskellPackagesFor`.
      configurations = import ./nix/haskell-configurations.nix { inherit inputs system; };

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
      defaultGhcVersion = "ghc90";

      # This is a shell utility that watches source files for changes, and triggers a
      # command when they change.
      watch-unwrapped = name: command:
        pkgs.writeScriptBin "${name}-unwrapped" ''
          hpack
          ${command}
        '';
      watch = name: command:
        pkgs.writeShellApplication {
          name = name;
          runtimeInputs = [ pkgs.fswatch pkgs.hpack (watch-unwrapped name command) ];
          text =
            let sources = builtins.concatStringsSep " " (import ./nix/haskell-source.nix); in
            ''
              fswatch --recursive --event-flags --event Created --event Removed --event Updated --event Renamed --event MovedFrom --event MovedTo --one-per-batch --latency=2 ${sources}  | sh -c "while read NUM; do ${name}-unwrapped; done;"
            '';
        };

      # Trigger a simple build every time a file changes
      build-watch = watch "build-watch" ''
        cabal build -f errors
      '';

      # Trigger a test execution every time a file changes
      # the --write-ghc-environment-files=always is required by doctest-parallel
      # see https://github.com/martijnbastiaan/doctest-parallel/blob/main/example/README.md#cabalproject
      # and https://github.com/martijnbastiaan/doctest-parallel/issues/22
      test-watch = watch "test-watch" ''
        cabal build --write-ghc-environment-files=always
        cabal test --test-show-details=streaming -f errors
      '';
    in
    rec {
      packages = {
        # Build crem for one given GHC versions.
        crem = foldConfigurations (haskellPackages: haskellPackages.crem);

        # Build crem for all GHC versions at once, collecting the results into one derivation.
        # documentation: https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/trivial-builders.nix
        cremAll = pkgs.symlinkJoin {
          name = "cremAll";
          paths = builtins.attrValues packages.crem;
        };

        default = packages.crem.${defaultGhcVersion};
      };

      # Prepare a development shell for many diffent GHC versions.
      devShells = foldConfigurations
        (haskellPackages:
          haskellPackages.shellFor {
            packages = ps: [ ps.crem ];
            nativeBuildInputs = with haskellPackages; [
              cabal-install
              fourmolu
              haskell-language-server
              build-watch
              test-watch
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
