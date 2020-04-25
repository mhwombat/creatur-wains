let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          project =
            haskellPackagesNew.callPackage ./project.nix { };
          creatur =
            haskellPackagesNew.callPackage ./creatur.nix { };
          som =
            haskellPackagesNew.callPackage ./som.nix { };
          grid =
            haskellPackagesNew.callPackage ./grid.nix { };
          gray-extended =
            haskellPackagesNew.callPackage ./gray-extended.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { project = pkgs.haskellPackages.callPackage ./project.nix { };
  }
