let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskellPackages.developPackage {
    root = ./.;
    source-overrides = {
      creatur = ../creatur;
      creatur-genes = ../creatur-genes;
      grid = ../grid;
      som = ../som;
    };
  }
