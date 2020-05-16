{ withHoogle ? false
, compiler ? "ghc8101"
}:
let
config = {
  doCheck = false;
  allowBroken = true;
  packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.ghc8101.override {
          overrides = (self: super: {
            microlens = pkgs.haskellPackages.callHackage "microlens" "0.4.11.2" {};
            ed25519 = pkgs.haskell.lib.overrideCabal super.ed25519 (drv: {
                doCheck = false;
                        patchPhase = ''
                          sed -i -e 's|ghc-prim .*$|ghc-prim ,|' ed25519.cabal
                        '';
              });
            cryptohash-sha256 = pkgs.haskell.lib.overrideCabal super.cryptohash-sha256 (drv: {
                doCheck = false;
                        patchPhase = ''
                          sed -i -e 's|base .*$|base|' cryptohash-sha256.cabal
                        '';
              });
            });
        };
    };
  };
pkgs = (import ./nixpkgs {inherit config;}).pkgs;
HsYAML = pkgs.haskell.lib.addBuildDepend pkgs.haskellPackages.HsYAML (with pkgs;
      [ haskellPackages.base_4_12_0_0 ]);
microlens = pkgs.haskellPackages.callHackage "microlens" "0.4.11.2" {};
#stack = pkgs.haskellPackages.callHackage "stack" "2.3.1" {};
ghc = pkgs.haskellPackages.ghc;

this = rec {
    inherit pkgs ghc;
  };
project =
pkgs.haskell.lib.buildStackProject {
  name = "inline-fortran";
# buildInputs = with pkgs; [ git protobuf zlib gcc.cc gfortran gfortran.cc haskellPackages.happy haskellPackages.alex gdb
# stack
# ];
  doHaddock = false;
  doCheck = false;

  };
in
this // project
