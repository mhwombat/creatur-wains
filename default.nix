let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskellPackages.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [
          cabal-install
        ]);
    source-overrides = {
      creatur = ../creatur;
      creatur-genes = ../creatur-genes;
      grid = ../grid;
      som = ../som;
    };
  }
