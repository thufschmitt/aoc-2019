{ pkgs ? import <nixpkgs> {} }:
pkgs.ocamlPackages.buildDunePackage rec {
  pname = "aoc";
  version = "2019";

  propagatedBuildInputs = [ pkgs.ocamlPackages.containers pkgs.ocamlPackages.gen ];

  src = pkgs.nix-gitignore.gitignoreSource [] ./.;
}
