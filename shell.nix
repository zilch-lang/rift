{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  name = "rift-shell";

  buildInputs = with pkgs; [
    dhall
    dhall-json
  ];
}
