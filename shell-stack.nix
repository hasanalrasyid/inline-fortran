{ withHoogle ? false
, compiler ? "ghc8101"
}:
let
config = {
  allowBroken = true;
  packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.ghc8101;
    };
  };
pkgs = (import ./nixpkgs {inherit config;}).pkgs;
ghc = pkgs.haskell.packages.ghc8101.ghcWithPackages (ps: with ps; []);
this = rec {
    inherit pkgs ghc;
  };
project =
pkgs.haskell.lib.buildStackProject {
  name = "inline-fortran";
  buildInputs = with pkgs; [ git hlint protobuf haskellPackages.stylish-haskell zlib gcc.cc gfortran gfortran.cc haskellPackages.happy haskellPackages.alex ghcid gdb ];
  doHaddock = false;
  doCheck = false;
};
in
this // project
