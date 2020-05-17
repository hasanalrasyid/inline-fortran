{ withHoogle ? false
, compiler ? "ghc8101"
}:
let
  fetchFromGitHub = { owner, repo, rev, sha256, branch }:
    builtins.fetchTarball {
        inherit sha256;
        url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
      };

config = {
  doCheck = false;
  allowBroken = true;
  packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.ghc8101.override {
          overrides = (self: super: {
            stack = pkgs.haskellPackages.callCabal2nix "stack" (
              builtins.fetchTarball {
                url = "https://github.com/commercialhaskell/stack/archive/9dcef52902d01646d63fe76fc8e6b1b3ac6cc9b8.tar.gz";
                sha256 = "0xyiaj3z0hm4zdkdird5296q5r9lb5dpqlm4352z9kglksq66k63";
            }){};
            });
        };
    };
  };
npkgs = (import ./nixpkgs {inherit config;});
pkgs = npkgs.pkgs;
ghc = pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [
  GenericPretty
  QuickCheck
  alex
  ansi
  fgl
  happy
  hashable
  inline
  json
  language
  lens
  parsers
  prettyprinter
  random
  raw
  split
  uniplate
  unordered
  vector
  ]);

this = rec {
    inherit pkgs ghc;
  };
project =
pkgs.haskell.lib.buildStackProject {
  name = "inline-fortran";
  buildInputs = with pkgs; [ git protobuf zlib gcc.cc gfortran gfortran.cc haskellPackages.happy haskellPackages.alex gdb 
  ];
  doHaddock = false;
  doCheck = false;

  };
in
this // project
