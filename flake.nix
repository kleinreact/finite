{
  description = "Nix flake for the finite package";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, flake-utils } :
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        ghcVersions = [ "ghc910" "ghc9121" ];
        defaultGhcVersion = "ghc9121";

        makeOverlay = compilerVersion:
          let
            overrideFile = ./. + "/nix/override-${compilerVersion}.nix";

            overrides =
              if builtins.pathExists overrideFile then
                import overrideFile { inherit pkgs; }
              else
                f: p: { };

            hsOverlay =
              pkgs.haskell.packages.${compilerVersion}.override {
                inherit overrides;
              };

            pkgSrcOverrides = pkgs.haskell.lib.compose.packageSourceOverrides {
              finite = ./.;
            };
          in
            hsOverlay.extend pkgSrcOverrides;

        overlays = nixpkgs.lib.attrsets.genAttrs ghcVersions makeOverlay;

        makeDevShell = compilerVersion:
          with overlays.${compilerVersion};
          shellFor {
            name = compilerVersion;
            packages = p: [
              # project packages
              p.finite
            ];
            nativeBuildInputs = [
              # Haskell dependencies
              # cabal-install
            ];
          };

        shells = pkgs.lib.attrsets.genAttrs ghcVersions makeDevShell;
      in {
        devShells = shells // { default = shells.${defaultGhcVersion}; };
      });
}
