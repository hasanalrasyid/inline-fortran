{ reflex-platform ? import ./reflex-platform {
  config.android_sdk.accept_license = true;
  config.allowBroken = true;
  config.extraoptions = "
    keep-outputs = true
    keep-derivations = true
  ";
  config.doHaddock = false;
  }
, withHoogle ? false
}:
with reflex-platform.nixpkgs;
let
pkgs = reflex-platform.nixpkgs;
ghc = pkgs.ghc;
this = rec {
    inherit pkgs;
  };
project =
pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "inline-fortran";
  buildInputs = [ git hlint protobuf haskellPackages.stylish-haskell zlib gcc.cc gfortran gfortran.cc haskellPackages.happy haskellPackages.alex ghcid gdb ];
  doHaddock = false;
  doCheck = false;
};
in
this // project
