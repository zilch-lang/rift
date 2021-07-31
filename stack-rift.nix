{
  pkgs ? import <nixpkgs> {}
, ghc ? pkgs.ghc
}:

pkgs.haskell.lib.buildStackProject {
  inherit ghc;

  nativeBuildInputs = with pkgs; [
    zlib
  ];

  name = "rift";
}
