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
nixpkgs = reflex-platform.nixpkgs;
ghc = nixpkgs.ghc;
in
nixpkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "inline-fortran";
  buildInputs = [ git hlint protobuf haskellPackages.stylish-haskell zlib gfortran gfortran.cc haskellPackages.language-rust ];
  doHaddock = false;
  doCheck = false;
}
