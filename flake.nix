{
  description = "twesterhout/dlpack-hs: Haskell interface for DLPack";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = inputs:
    let
      inherit (inputs.nixpkgs) lib;

      src = inputs.nix-filter.lib {
        root = ./.;
        include = [
          "src"
          "example"
          "test"
          "dlpack-hs.cabal"
          "README.md"
          "LICENSE"
        ];
      };

      overlay = self: super: {
        haskell = super.haskell // {
          packageOverrides = lib.composeExtensions super.haskell.packageOverrides
            (hself: hsuper: {
              dlpack-hs = hself.callCabal2nix "dlpack-hs" src { };
            });
        };
      };

      devShellFor = pkgs:
        let
          ps = pkgs.haskellPackages;
        in
        ps.shellFor {
          packages = ps: with ps; [ dlpack-hs ];
          withHoogle = true;
          nativeBuildInputs = with pkgs; with ps; [
            cabal-install
            doctest
            markdown-unlit
            haskell-language-server
            fourmolu
            python3Packages.grip
          ];
        };

      pkgsFor = system: import inputs.nixpkgs {
        inherit system;
        overlays = [ overlay ];
      };
    in
    {
      packages = inputs.flake-utils.lib.eachDefaultSystemMap (system:
        with pkgsFor system; {
          default = haskellPackages.dlpack-hs;
          haskell = haskell.packages;
        });

      devShells = inputs.flake-utils.lib.eachDefaultSystemMap (system: {
        default = devShellFor (pkgsFor system);
      });

      overlays.default = overlay;
    };
}
