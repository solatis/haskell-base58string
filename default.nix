{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:
nixpkgs.haskellPackages.callPackage ./base58string.nix { }
