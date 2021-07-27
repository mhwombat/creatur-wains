let 
  pkgs = import <nixpkgs> { };
in 
  pkgs.haskellPackages.developPackage {
    root = ./.;
    source-overrides = {
      creatur = ../creatur;
      gray-extended = ../gray-extended;
      grid = ../grid;
      som = ../som;
    };
  }
