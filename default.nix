let 
  pkgs = import <nixpkgs> { };
in 
  pkgs.haskellPackages.developPackage {
    root = ./.;
    source-overrides = {
      creatur = ../creatur;
      creatur-genes = ../creatur-genes;
      gray-extended = ../gray-extended;
      grid = ../grid;
      som = ../som;
    };
  }
