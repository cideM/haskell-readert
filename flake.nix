{
  description = "A minimal example showing how to structure your Haskell application with the ReaderT IO pattern";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      haskellProject = pkgs.haskellPackages.callPackage ./project.nix { };
    in
    rec {
      packages = flake-utils.lib.flattenTree ({ exampleApp = haskellProject; });
      defaultPackage = packages.exampleApp;
      apps.exampleApp = flake-utils.lib.mkApp { drv = packages.exampleApp; };
      defaultApp = apps.exampleApp;
      devShell = pkgs.mkShell {
        inputsFrom = [ haskellProject.env ];
        buildInputs = with pkgs.haskellPackages;
          [
            # Haskell
            ghcid
            ormolu
            hlint
            cabal2nix
            haskell-language-server
            cabal-install
            cabal-fmt
            fast-tags
            hoogle
          ];
      };
    }
  );
}
