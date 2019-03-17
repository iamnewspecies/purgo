{ compiler ? "ghc843", hostpkgs ? import <nixpkgs> { } }:
let
  pkgs = import ./nix/nixpkgs.nix { bootstrap = hostpkgs; };

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      # Pin the version of PureScript that we want (for testing).
      purescript = self.callPackage ./nix/purescript.nix { };
    };
  };

in

pkgs.mkShell rec {
  buildInputs = [
    haskellPackages.ghc
    haskellPackages.cabal-install
    haskellPackages.purescript
    haskellPackages.stylish-haskell
    pkgs.zlib
    pkgs.gnumake
  ];

  LD_LIBRARY_PATH = pkgs.stdenv.lib.makeLibraryPath [ pkgs.zlib ]; # buildInputs
}
