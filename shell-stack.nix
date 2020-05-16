{ withHoogle ? false
, compiler ? "ghc8101"
}:
let
config = {
  doCheck = false;
  allowBroken = true;
  packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.ghc8101;
    };
  };
pkgs = (import ./nixpkgs {inherit config;}).pkgs;
ghc = pkgs.haskell.packages.ghc8101.ghcWithPackages (ps: with ps; []);
HsYAML = pkgs.haskell.lib.addBuildDepend pkgs.haskellPackages.HsYAML (with pkgs;
      [ haskellPackages.base_4_12_0_0 ]);

this = rec {
    inherit pkgs ghc;
  };
project =
pkgs.haskell.lib.buildStackProject {
  name = "inline-fortran";
  buildInputs = with pkgs; [ git hlint protobuf zlib gcc.cc gfortran gfortran.cc haskellPackages.happy haskellPackages.alex gdb
  ];
  doHaddock = false;
  doCheck = false;

  };
in
this // project
