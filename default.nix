{ nixpkgs ? import ./nix/nixos-20-03.nix }:
let
  overlay = self: super: {
    myHaskellPackages = 
      super.haskell.packages.ghc865.override (old: {
        overrides = self.lib.composeExtensions (old.overrides or (_: _: {})) 
          (hself: hsuper: {
            ghc = hsuper.ghc // { withPackages = hsuper.ghc.withHoogle; };
            ghcWithPackages = hself.ghc.withPackages;
            grid = self.haskell.lib.dontCheck (hself.callCabal2nix "grid" /home/amy/github/grid {});
            som = self.haskell.lib.dontCheck (hself.callCabal2nix "som" /home/amy/github/som {});
            creatur = self.haskell.lib.dontCheck (hself.callCabal2nix "creatur" /home/amy/github/creatur {});
            lens = self.haskell.lib.dontCheck 
              (hself.callHackageDirect 
                { pkg = "lens";
                  ver = "4.19.2";
                  sha256 = "0cgkigb7p0igzg9l669xkq787bb1cw32lx03pcgv5ivd6zsx3fpm";
                } {}
              );
          });
      });
  };

  pkgs = import nixpkgs {
    overlays = [overlay];
  };

  drv = pkgs.myHaskellPackages.callCabal2nix "creatur-wains" ./creatur-wains.cabal {};

  drvWithTools = drv.env.overrideAttrs (
    old: with pkgs.myHaskellPackages; {
      nativeBuildInputs = old.nativeBuildInputs ++ [
        ghcid
        # Add other development tools like ormolu here
      ];
      shellHook = ''
         source .config/secrets     '';
      }
  );
in
  drvWithTools
