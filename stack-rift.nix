{ pkgs ? import <nixpkgs> { }
, ghc ? pkgs.ghc
}:

pkgs.haskell.lib.buildStackProject {
  inherit ghc;

  nativeBuildInputs = with pkgs; [
    zlib

    git
    dhall-json
    dhall
  ];

  name = "rift";
}
